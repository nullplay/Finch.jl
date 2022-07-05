@kwdef mutable struct Run
    body
end

Base.show(io::IO, ex::Run) = Base.show(io, MIME"text/plain"(), ex)
function Base.show(io::IO, mime::MIME"text/plain", ex::Run)
    print(io, "Run(body = ")
    println(ex.body)
    print(io, ")")
end

isliteral(::Run) = false

#A minor revelation: There's no reason to store extents in chunks, they just modify the extents of the context.

getname(arr::Run) = getname(arr.body)

struct RunStyle end

(ctx::Stylize{LowerJulia})(node::Run) = RunStyle()
combine_style(a::DefaultStyle, b::RunStyle) = RunStyle()
combine_style(a::ThunkStyle, b::RunStyle) = ThunkStyle()
combine_style(a::SimplifyStyle, b::RunStyle) = SimplifyStyle()
combine_style(a::RunStyle, b::RunStyle) = RunStyle()

function (ctx::LowerJulia)(root::Chunk, ::RunStyle)
    root = (AccessRunVisitor(root))(root)
    if Stylize(root, ctx)(root) isa RunStyle #TODO do we need this always? Can we do this generically?
        error("run style couldn't lower runs")
    end
    ctx(root)
end

@kwdef struct AccessRunVisitor
    root
end
function (ctx::AccessRunVisitor)(node)
    if istree(node)
        return similarterm(node, operation(node), map(ctx, arguments(node)))
    else
        return node
    end
end

function (ctx::AccessRunVisitor)(node::Access{Run, Read})
    return node.tns.body
end

function (ctx::AccessRunVisitor)(node::Access{Shift, Read}) #TODO this is ugly
    if node.tns.body isa Run
        return node.tns.body.body
    else
        return node
    end
end


#assume ssa

@kwdef mutable struct AcceptRun
    body
end

function Base.show(io::IO, mime::MIME"text/plain", ex::AcceptRun)
    print(io, "AcceptRun()")
end

struct AcceptRunStyle end

(ctx::Stylize{LowerJulia})(node::AcceptRun) = AcceptRunStyle()
combine_style(a::DefaultStyle, b::AcceptRunStyle) = AcceptRunStyle()
combine_style(a::ThunkStyle, b::AcceptRunStyle) = ThunkStyle()
combine_style(a::SimplifyStyle, b::AcceptRunStyle) = SimplifyStyle()
combine_style(a::AcceptRunStyle, b::AcceptRunStyle) = AcceptRunStyle()
combine_style(a::RunStyle, b::AcceptRunStyle) = RunStyle()

function (ctx::LowerJulia)(root::Chunk, ::AcceptRunStyle)
    body = (AcceptRunVisitor(root, root.idx, root.ext, ctx))(root.body)
    if getname(root.idx) in getunbound(body)
        #call DefaultStyle, the only style that AcceptRunStyle promotes with
        return ctx(root, DefaultStyle())
    else
        return ctx(body)
    end
end

@kwdef struct AcceptRunVisitor
    root
    idx
    ext
    ctx
end

function (ctx::AcceptRunVisitor)(node)
    if istree(node)
        return similarterm(node, operation(node), map(ctx, arguments(node)))
    else
        return node
    end
end

function (ctx::AcceptRunVisitor)(node::Access{AcceptRun, <:Union{Write, Update}})
    node.tns.body(ctx.ctx, getstart(ctx.ext), getstop(ctx.ext))
end

function (ctx::ForLoopVisitor)(node::Access{AcceptRun, <:Union{Write, Update}})
    node.tns.body(ctx.ctx, ctx.val, ctx.val)
end

function (ctx::AcceptRunVisitor)(node::Access{Shift}, ::DefaultStyle)
    ctx_2 = AcceptRunVisitor(ctx.root, ctx.idx, call(-, ctx.val, node.shift), shiftdim(ctx.ext, call(-, node.shift)))
    ctx_2(access(node.tns.body, node.mode, node.idxs...))
end

supports_shift(::RunStyle) = true
supports_shift(::AcceptRunStyle) = true