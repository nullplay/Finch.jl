begin
    C_lvl = ex.body.lhs.tns.tns.lvl
    C_lvl_2 = C_lvl.lvl
    C_lvl_2_val = 0.0
    A_lvl = (ex.body.rhs.args[1]).tns.tns.lvl
    A_lvl_2 = A_lvl.lvl
    A_lvl_2_val = 0.0
    B_lvl = (ex.body.rhs.args[2]).tns.tns.lvl
    B_lvl_2 = B_lvl.lvl
    B_lvl_2_val = 0.0
    C_lvl_qos_fill = 0
    C_lvl_qos_stop = 0
    (Finch.resize_if_smaller!)(C_lvl.pos, 1 + 1)
    (Finch.fill_range!)(C_lvl.pos, 0, 1 + 1, 1 + 1)
    C_lvl_qos = C_lvl_qos_fill + 1
    B_lvl_q = B_lvl.pos[1]
    B_lvl_q_stop = B_lvl.pos[1 + 1]
    B_lvl_i = if B_lvl_q < B_lvl_q_stop
            B_lvl.idx[B_lvl_q]
        else
            1
        end
    B_lvl_i1 = if B_lvl_q < B_lvl_q_stop
            B_lvl.idx[B_lvl_q_stop - 1]
        else
            0
        end
    A_lvl_q = A_lvl.pos[1]
    A_lvl_q_stop = A_lvl.pos[1 + 1]
    A_lvl_i = if A_lvl_q < A_lvl_q_stop
            A_lvl.idx[A_lvl_q]
        else
            1
        end
    A_lvl_i1 = if A_lvl_q < A_lvl_q_stop
            A_lvl.idx[A_lvl_q_stop - 1]
        else
            0
        end
    i = 1
    i_start = i
    phase_stop = (min)(A_lvl.I, A_lvl_i1, B_lvl_i1)
    if phase_stop >= i_start
        i = i
        i = i_start
        while i <= phase_stop
            i_start_2 = i
            while B_lvl_q + 1 < B_lvl_q_stop && B_lvl.idx[B_lvl_q] < i_start_2
                B_lvl_q += 1
            end
            B_lvl_i = B_lvl.idx[B_lvl_q]
            while A_lvl_q + 1 < A_lvl_q_stop && A_lvl.idx[A_lvl_q] < i_start_2
                A_lvl_q += 1
            end
            A_lvl_i = A_lvl.idx[A_lvl_q]
            phase_stop_2 = (min)((max)(A_lvl_i, B_lvl_i), phase_stop)
            if phase_stop_2 >= i_start_2
                i_2 = i
                if phase_stop_2 == B_lvl_i && phase_stop_2 == A_lvl_i
                    B_lvl_2_val = B_lvl_2.val[B_lvl_q]
                    A_lvl_2_val = A_lvl_2.val[A_lvl_q]
                    i_3 = phase_stop_2
                    if C_lvl_qos > C_lvl_qos_stop
                        C_lvl_qos_stop = max(C_lvl_qos_stop << 1, 1)
                        (Finch.resize_if_smaller!)(C_lvl.idx, C_lvl_qos_stop)
                        resize_if_smaller!(C_lvl_2.val, C_lvl_qos_stop)
                        fill_range!(C_lvl_2.val, 0.0, C_lvl_qos, C_lvl_qos_stop)
                    end
                    C_lvl_2_dirty = false
                    C_lvl_2_val = C_lvl_2.val[C_lvl_qos]
                    C_lvl_2_dirty = true
                    C_lvl_2_dirty = true
                    C_lvl_2_val = (+)(C_lvl_2_val, (*)(A_lvl_2_val, B_lvl_2_val))
                    C_lvl_2.val[C_lvl_qos] = C_lvl_2_val
                    if C_lvl_2_dirty
                        C_lvl_dirty = true
                        C_lvl.idx[C_lvl_qos] = i_3
                        C_lvl_qos += 1
                    end
                    B_lvl_q += 1
                    A_lvl_q += 1
                elseif phase_stop_2 == A_lvl_i
                    A_lvl_2_val = A_lvl_2.val[A_lvl_q]
                    i = phase_stop_2
                    while B_lvl_q + 1 < B_lvl_q_stop && B_lvl.idx[B_lvl_q] < phase_stop_2
                        B_lvl_q += 1
                    end
                    i_start_3 = i
                    B_lvl_i = B_lvl.idx[B_lvl_q]
                    phase_stop_3 = (min)(B_lvl_i, phase_stop_2)
                    i_4 = i
                    if B_lvl_i == phase_stop_3
                        B_lvl_2_val = B_lvl_2.val[B_lvl_q]
                        i_5 = phase_stop_3
                        if C_lvl_qos > C_lvl_qos_stop
                            C_lvl_qos_stop = max(C_lvl_qos_stop << 1, 1)
                            (Finch.resize_if_smaller!)(C_lvl.idx, C_lvl_qos_stop)
                            resize_if_smaller!(C_lvl_2.val, C_lvl_qos_stop)
                            fill_range!(C_lvl_2.val, 0.0, C_lvl_qos, C_lvl_qos_stop)
                        end
                        C_lvl_2_dirty = false
                        C_lvl_2_val = C_lvl_2.val[C_lvl_qos]
                        C_lvl_2_dirty = true
                        C_lvl_2_dirty = true
                        C_lvl_2_val = (+)(C_lvl_2_val, (*)(A_lvl_2_val, B_lvl_2_val))
                        C_lvl_2.val[C_lvl_qos] = C_lvl_2_val
                        if C_lvl_2_dirty
                            C_lvl_dirty = true
                            C_lvl.idx[C_lvl_qos] = i_5
                            C_lvl_qos += 1
                        end
                        B_lvl_q += 1
                    else
                    end
                    i = phase_stop_3 + 1
                    A_lvl_q += 1
                elseif phase_stop_2 == B_lvl_i
                    B_lvl_2_val = B_lvl_2.val[B_lvl_q]
                    i = phase_stop_2
                    while A_lvl_q + 1 < A_lvl_q_stop && A_lvl.idx[A_lvl_q] < phase_stop_2
                        A_lvl_q += 1
                    end
                    i_start_4 = i
                    A_lvl_i = A_lvl.idx[A_lvl_q]
                    phase_stop_4 = (min)(A_lvl_i, phase_stop_2)
                    i_6 = i
                    if A_lvl_i == phase_stop_4
                        A_lvl_2_val = A_lvl_2.val[A_lvl_q]
                        i_7 = phase_stop_4
                        if C_lvl_qos > C_lvl_qos_stop
                            C_lvl_qos_stop = max(C_lvl_qos_stop << 1, 1)
                            (Finch.resize_if_smaller!)(C_lvl.idx, C_lvl_qos_stop)
                            resize_if_smaller!(C_lvl_2.val, C_lvl_qos_stop)
                            fill_range!(C_lvl_2.val, 0.0, C_lvl_qos, C_lvl_qos_stop)
                        end
                        C_lvl_2_dirty = false
                        C_lvl_2_val = C_lvl_2.val[C_lvl_qos]
                        C_lvl_2_dirty = true
                        C_lvl_2_dirty = true
                        C_lvl_2_val = (+)((*)(B_lvl_2_val, A_lvl_2_val), C_lvl_2_val)
                        C_lvl_2.val[C_lvl_qos] = C_lvl_2_val
                        if C_lvl_2_dirty
                            C_lvl_dirty = true
                            C_lvl.idx[C_lvl_qos] = i_7
                            C_lvl_qos += 1
                        end
                        A_lvl_q += 1
                    else
                    end
                    i = phase_stop_4 + 1
                    B_lvl_q += 1
                else
                    i = i_start_2
                    while B_lvl_q + 1 < B_lvl_q_stop && B_lvl.idx[B_lvl_q] < i_start_2
                        B_lvl_q += 1
                    end
                    while A_lvl_q + 1 < A_lvl_q_stop && A_lvl.idx[A_lvl_q] < i_start_2
                        A_lvl_q += 1
                    end
                    while i <= phase_stop_2
                        i_start_5 = i
                        B_lvl_i = B_lvl.idx[B_lvl_q]
                        A_lvl_i = A_lvl.idx[A_lvl_q]
                        phase_stop_5 = (min)(A_lvl_i, B_lvl_i, phase_stop_2)
                        i_8 = i
                        if B_lvl_i == phase_stop_5 && A_lvl_i == phase_stop_5
                            B_lvl_2_val = B_lvl_2.val[B_lvl_q]
                            A_lvl_2_val = A_lvl_2.val[A_lvl_q]
                            i_9 = phase_stop_5
                            if C_lvl_qos > C_lvl_qos_stop
                                C_lvl_qos_stop = max(C_lvl_qos_stop << 1, 1)
                                (Finch.resize_if_smaller!)(C_lvl.idx, C_lvl_qos_stop)
                                resize_if_smaller!(C_lvl_2.val, C_lvl_qos_stop)
                                fill_range!(C_lvl_2.val, 0.0, C_lvl_qos, C_lvl_qos_stop)
                            end
                            C_lvl_2_dirty = false
                            C_lvl_2_val = C_lvl_2.val[C_lvl_qos]
                            C_lvl_2_dirty = true
                            C_lvl_2_dirty = true
                            C_lvl_2_val = (+)((*)(B_lvl_2_val, A_lvl_2_val), C_lvl_2_val)
                            C_lvl_2.val[C_lvl_qos] = C_lvl_2_val
                            if C_lvl_2_dirty
                                C_lvl_dirty = true
                                C_lvl.idx[C_lvl_qos] = i_9
                                C_lvl_qos += 1
                            end
                            B_lvl_q += 1
                            A_lvl_q += 1
                        elseif A_lvl_i == phase_stop_5
                            A_lvl_2_val = A_lvl_2.val[A_lvl_q]
                            A_lvl_q += 1
                        elseif B_lvl_i == phase_stop_5
                            B_lvl_2_val = B_lvl_2.val[B_lvl_q]
                            B_lvl_q += 1
                        else
                        end
                        i = phase_stop_5 + 1
                    end
                end
                i = phase_stop_2 + 1
            end
        end
        i = phase_stop + 1
    end
    i_start = i
    phase_stop_6 = (min)(A_lvl.I, B_lvl_i1)
    if phase_stop_6 >= i_start
        i_10 = i
        i = phase_stop_6 + 1
    end
    i_start = i
    phase_stop_7 = (min)(A_lvl.I, A_lvl_i1)
    if phase_stop_7 >= i_start
        i_11 = i
        i = phase_stop_7 + 1
    end
    i_start = i
    if A_lvl.I >= i_start
        i_12 = i
        i = A_lvl.I + 1
    end
    C_lvl.pos[1 + 1] = (C_lvl_qos - C_lvl_qos_fill) - 1
    C_lvl_qos_fill = C_lvl_qos - 1
    for p = 2:1 + 1
        C_lvl.pos[p] += C_lvl.pos[p - 1]
    end
    qos_stop = C_lvl.pos[1 + 1] - 1
    resize!(C_lvl.pos, 1 + 1)
    qos = C_lvl.pos[end] - 1
    resize!(C_lvl.idx, qos)
    resize!(C_lvl_2.val, qos)
    (C = Fiber((Finch.SparseListLevel){Int64}(A_lvl.I, C_lvl.pos, C_lvl.idx, C_lvl_2), (Environment)(; )),)
end
