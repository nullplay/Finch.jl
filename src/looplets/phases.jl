@kwdef struct Phase
    head = nothing
    body
    stride = (ctx, ext) -> nothing
    range = (ctx, ext) -> Extent(start = getstart(ext), stop = something(stride(ctx, ext), getstop(ext)))
end
FinchNotation.finch_leaf(x::Phase) = virtual(x)

Base.show(io::IO, ex::Phase) = Base.show(io, MIME"text/plain"(), ex)
function Base.show(io::IO, mime::MIME"text/plain", ex::Phase)
    print(io, "Phase()")
end

@kwdef struct PhaseStride
    ctx
    ext
end

function (ctx::PhaseStride)(node)
    if istree(node)
        return mapreduce(ctx, (a, b) -> resultdim(ctx.ctx, a, b), arguments(node), init = nodim)
    else
        return nodim
    end
end

# Basically Intersecting all dimensions of below this node

# mapreduce(f, op, itrs...; [init]) = reduce(op, map(f, itr); init=init)
function (ctx::PhaseStride)(node::FinchNode)
    if node.kind === virtual
        ctx(node.val)
    elseif istree(node)
        return mapreduce(ctx, (a, b) -> resultdim(ctx.ctx, a, b), arguments(node), init = nodim)
    else
        return nodim
    end
end

(ctx::PhaseStride)(node::Phase) = Narrow(node.range(ctx.ctx, ctx.ext))
(ctx::PhaseStride)(node::Shift) = shiftdim(PhaseStride(;kwfields(ctx)..., ext = shiftdim(ctx.ext, call(-, node.delta)))(node.body), node.delta)

@kwdef struct PhaseBodyVisitor
    ctx
    ext
    ext_2
end

function (ctx::PhaseBodyVisitor)(node)
    if istree(node)
        return similarterm(node, operation(node), map(ctx, arguments(node)))
    else
        return node
    end
end
function (ctx::PhaseBodyVisitor)(node::FinchNode)
    if node.kind === virtual
        ctx(node.val)
    elseif istree(node)
        return similarterm(node, operation(node), map(ctx, arguments(node)))
    else
        return node
    end
end

(ctx::PhaseBodyVisitor)(node::Phase) = node.body(ctx.ctx, ctx.ext_2)
(ctx::PhaseBodyVisitor)(node::Spike) = truncate(node, ctx.ctx, ctx.ext, ctx.ext_2) #TODO This should be called on everything

(ctx::PhaseBodyVisitor)(node::Shift) = Shift(PhaseBodyVisitor(ctx.ctx, shiftdim(ctx.ext, call(-, node.delta)), shiftdim(ctx.ext_2, call(-, node.delta)))(node.body), node.delta)

struct PhaseStyle end

supports_shift(::PhaseStyle) = true

(ctx::Stylize{LowerJulia})(node::Phase) = ctx.root.kind === chunk ? PhaseStyle() : DefaultStyle()

combine_style(a::DefaultStyle, b::PhaseStyle) = PhaseStyle()
combine_style(a::PhaseStyle, b::PhaseStyle) = PhaseStyle()
combine_style(a::PhaseStyle, b::RunStyle) = PhaseStyle()
combine_style(a::PhaseStyle, b::SpikeStyle) = PhaseStyle()
combine_style(a::SimplifyStyle, b::PhaseStyle) = a
combine_style(a::AcceptRunStyle, b::PhaseStyle) = PhaseStyle()
combine_style(a::SwitchStyle, b::PhaseStyle) = SwitchStyle()
combine_style(a::ThunkStyle, b::PhaseStyle) = ThunkStyle()

function (ctx::LowerJulia)(root::FinchNode, ::PhaseStyle)
    if root.kind === chunk
        i = getname(root.idx)
        i0=ctx.freshen(i)
        body = root.body

        # ext_2 : current phase's stride (last phase)
        # body = Top node of AST that includes phase() below itself.
        ext_2 = resolvedim(PhaseStride(ctx, root.ext)(body))  #resolvedim = ext.ext
        println("[Debug] ", "[", ctx(getstart(ext_2)), ", ", ctx(getstop(ext_2)), ") & [",ctx(getstart(root.ext)), ", ", ctx(getstop(root.ext)), ")" )
        #println("[Debug2] ", ctx(getstop(resolvedim(resultdim(ctx, Narrow(root.ext), ext_2)))))
        
        # root.ext = phase_stop_2 (minimum of stepper and last phase stop)
        ext_2 = cache_dim!(ctx, :phase, resolvedim(resultdim(ctx, Narrow(root.ext), ext_2))) # assignment of phase_stop

        body = PhaseBodyVisitor(ctx, root.ext, ext_2)(body)
        body = quote
            $i0 = $i
            $(contain(ctx) do ctx_4
                (ctx_4)(chunk(
                    root.idx,
                    ext_2,
                    body
                ))
            end)
            #[wjy] not sure if this is a right decision
            #$i = $(ctx(getstop(ext_2))) + $(Int8(1))
            $i = $(ctx(getstop(ext_2))) 
        end

        if query(call(>, measure(ext_2), 0), ctx)
            return body
        else
            return quote
                #[wjy] not sure if this is a right decision
                if $(ctx(getstop(ext_2))) > $(ctx(getstart(ext_2)))
                    $body
                end
            end
        end
    else
        error("unimplemented")
    end
end
