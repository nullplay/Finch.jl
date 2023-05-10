struct StepperStyle end

@kwdef struct Stepper
    body
    seek = (ctx, start) -> error("seek not implemented error")
end

Base.show(io::IO, ex::Stepper) = Base.show(io, MIME"text/plain"(), ex)
function Base.show(io::IO, mime::MIME"text/plain", ex::Stepper)
    print(io, "Stepper()")
end

FinchNotation.finch_leaf(x::Stepper) = virtual(x)

(ctx::Stylize{LowerJulia})(node::Stepper) = ctx.root.kind === chunk ? StepperStyle() : DefaultStyle()

combine_style(a::DefaultStyle, b::StepperStyle) = StepperStyle()
combine_style(a::StepperStyle, b::PipelineStyle) = PipelineStyle()
combine_style(a::StepperStyle, b::StepperStyle) = StepperStyle()
combine_style(a::StepperStyle, b::RunStyle) = RunStyle()
combine_style(a::SimplifyStyle, b::StepperStyle) = a
combine_style(a::StepperStyle, b::AcceptRunStyle) = StepperStyle()
combine_style(a::StepperStyle, b::SpikeStyle) = SpikeStyle()
combine_style(a::StepperStyle, b::SwitchStyle) = SwitchStyle()
combine_style(a::ThunkStyle, b::StepperStyle) = ThunkStyle()
combine_style(a::StepperStyle, b::JumperStyle) = JumperStyle()
combine_style(a::StepperStyle, b::PhaseStyle) = PhaseStyle()

function (ctx::LowerJulia)(root::FinchNode, style::StepperStyle)
    if root.kind === chunk
        return lower_cycle(root, ctx, root.idx, root.ext, style)
    else
        error("unimplemented")
    end
end

function (ctx::CycleVisitor{StepperStyle})(node::Stepper)
    push!(ctx.ctx.preamble, node.seek(ctx.ctx, ctx.ext))
    node.body
end

@kwdef struct Step
    stride
    next = (ctx, ext) -> quote end
    chunk = nothing
    # ext_2 : [start, min(steps)]
    # ext : extent of above scope
    body = (ctx, ext, ext_2) -> Thunk(
                                      body = Pipeline([ Phase(body = (ctx, ext) -> chunk)]),
                                      epilogue = quote 
                                        if $(ctx(stride(ctx, ext))) == $(ctx(getstop(ext_2))) 
                                          $(next(ctx, ext_2))
                                        end
                                      end
                                     )


    #Switch([
    #    value(:($(ctx(stride(ctx, ext))) == $(ctx(getstop(ext_2))))) => Thunk(
    #        body = truncate_weak(chunk, ctx, ext, ext_2),
    #        epilogue = next(ctx, ext_2)
    #    ),
    #    literal(true) => 
    #        truncate_strong(chunk, ctx, ext, ext_2),
    #    ])
end

FinchNotation.finch_leaf(x::Step) = virtual(x)

# Step is lowered by PhaseStyle
(ctx::Stylize{LowerJulia})(node::Step) = ctx.root.kind === chunk ? PhaseStyle() : DefaultStyle()

(ctx::PhaseStride)(node::Step) = begin
  println("[Step] ")
    s = node.stride(ctx.ctx, ctx.ext) #why is stride a function?
    Narrow(Extent(start = getstart(ctx.ext), stop = call(cached, s, call(max, s, call(+, getstart(ctx.ext), 1))), lower = literal(1)))
    #Narrow(Extent(start = getstart(ctx.ext), stop = call(cached, s, call(max, s, getstart(ctx.ext))), lower = literal(1)))
end

(ctx::PhaseBodyVisitor)(node::Step) = node.body(ctx.ctx, ctx.ext, ctx.ext_2)

supports_shift(::StepperStyle) = true
