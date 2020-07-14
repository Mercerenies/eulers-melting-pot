
class
    PROBLEM84

create
    make

feature

    standard_movement: ARRAY [DOUBLE]
    space_resolution: ARRAY [DOUBLE]
    transition_matrix: ARRAY [DOUBLE]
    pivots: ARRAY [INTEGER]
    results: ARRAY [DOUBLE]

    make
        local
            v: DOUBLE
            ii: INTEGER
            pivot: INTEGER
            biggest1: INTEGER
            biggest2: INTEGER
            biggest3: INTEGER
            fmt: FORMAT_INTEGER
        do
            create standard_movement.make_filled(0, 0, 120 * 120 - 1)
            create space_resolution.make_filled(0, 0, 120 * 120 - 1)
            create transition_matrix.make_filled(0, 0, 120 * 120 - 1)
            create pivots.make_filled(0, 0, 120 - 1)
            create results.make_filled(1, 0, 120 - 1)

            across
                0 |..| 119 as r
            loop
                across
                    0 |..| 119 as c
                loop
                    v := 0
                    if r.item < 40 then
                        inspect mod(r.item - c.item, 40)
                        when 3, 7 then v := v + 2.0 / 16
                        when 4, 6 then v := v + 2.0 / 16
                        when 5    then v := v + 4.0 / 16
                        else
                        end
                        if c.item >= 80 and r.item = 10 then
                            v := v + 4.0 / 16
                        end
                    else
                        if (r.item / 40).floor = (c.item / 40).floor + 1 then
                            inspect mod(r.item - c.item, 40)
                            when 2, 4, 6, 8 then v := v + 1.0 / 16
                            else
                            end
                        end
                    end
                    set(standard_movement, r.item, c.item, v)
                end
            end

            across
                0 |..| 119 as r
            loop
                across
                    0 |..| 119 as c
                loop
                    v := 0
                    if (r.item / 40).floor = (c.item / 40).floor then
                        inspect c.item
                        when 30 then
                            if r.item = 10 then v := 1.0 else v := 0.0 end
                        when 2, 17, 33 then
                            inspect r.item
                            when 0 then v := 1.0 / 16.0
                            when 10 then v := 1.0 / 16.0
                            else if r.item = c.item then v := 14.0 / 16.0 else v := 0.0 end
                            end
                        when 7, 22, 36 then
                            inspect r.item
                            when 0, 10, 11, 24, 39, 5 then v := v + 1.0 / 16.0
                            else
                            end
                            if mod(r.item, 10) = 5 and mod(r.item - c.item, 40) < 10 then
                                v := v + 2.0 / 16.0
                            end
                            inspect c.item
                            when 7, 36 then
                                if r.item = 12 then v := v + 1.0 / 16.0 end
                            when 22 then
                                if r.item = 28 then v := v + 1.0 / 16.0 end
                            else
                            end
                            if r.item - c.item = -3 then
                                v := v + 1.0 / 16.0
                            end
                            if r.item = c.item then
                                v := v + 6.0 / 16.0
                            end
                        else
                            if r.item = c.item then v := 1.0 else v := 0.0 end
                        end
                    end
                    set(space_resolution, r.item, c.item, v)
                end
            end

            matmul(transition_matrix, space_resolution, standard_movement)
            across
                0 |..| 119 as i
            loop
                set(transition_matrix, i.item, i.item, get(transition_matrix, i.item, i.item) - 1)
            end
            gauss(transition_matrix)
            find_pivots(pivots, transition_matrix)

            across
                0 |..| 119 as ireversed
            loop
                ii := 119 - ireversed.item
                pivot := pivots.at(ii)
                if pivot >= 0 then
                    v := 0
                    across
                        (pivot + 1) |..| 119 as j
                    loop
                        v := v - get(transition_matrix, ii, j.item) * results.at(j.item)
                    end
                    v := v / get(transition_matrix, ii, pivot)
                    results.put(v, pivot)
                end
            end

            -- Normalize
            v := 0.0
            across
                0 |..| 119 as i
            loop
                v := v + results.at(i.item)
            end
            across
                0 |..| 119 as i
            loop
                results.put(results.at(i.item) / v, i.item)
            end
            across
                0 |..| 39 as i
            loop
                v := results.at(i.item) + results.at(i.item + 40) + results.at(i.item + 80)
                results.put(v, i.item)
                results.put(0, i.item + 40)
                results.put(0, i.item + 80)
            end

            biggest1 := -1
            across
                0 |..| 39 as i
            loop
                if (biggest1 = -1) or results.at(biggest1) < results.at(i.item) then
                    biggest1 := i.item
                end
            end

            biggest2 := -1
            across
                0 |..| 39 as i
            loop
                if not (i.item = biggest1) then
                    if (biggest2 = -1) or results.at(biggest2) < results.at(i.item) then
                        biggest2 := i.item
                    end
                end
            end

            biggest3 := -1
            across
                0 |..| 39 as i
            loop
                if not (i.item = biggest1) and not (i.item = biggest2) then
                    if (biggest3 = -1) or results.at(biggest3) < results.at(i.item) then
                        biggest3 := i.item
                    end
                end
            end

            create fmt.make(2)
            fmt.zero_fill
            io.put_string (fmt.formatted(biggest1))
            io.put_string (fmt.formatted(biggest2))
            io.put_string (fmt.formatted(biggest3))
            io.new_line

        end

feature

    gauss (a: ARRAY [DOUBLE])
        local
            h, k, m, n, i_max: INTEGER
            tmp, f: DOUBLE
        do
            h := 0
            k := 0
            m := 120
            n := 120
            from
            until
                h >= m or k >= n
            loop
                i_max := h
                across
                    h |..| (m - 1) as i
                loop
                    if abs(get(a, i.item, k)) > abs(get(a, i_max, k)) then
                        i_max := i.item
                    end
                end
                if get(a, i_max, k) = 0 then
                    k := k + 1
                else
                    -- Swap rows
                    across
                        0 |..| (n - 1) as j
                    loop
                        tmp := get(a, h, j.item)
                        set(a, h, j.item, get(a, i_max, j.item))
                        set(a, i_max, j.item, tmp)
                    end
                    across
                        (h + 1) |..| (m - 1) as i
                    loop
                        f := get(a, i.item, k) / get(a, h, k)
                        set(a, i.item, k, 0)
                        across
                            (k + 1) |..| (n - 1) as j
                        loop
                            set(a, i.item, j.item, get(a, i.item, j.item) - get(a, h, j.item) * f)
                        end
                    end
                    h := h + 1
                    k := k + 1
                end
            end
        end

feature

    find_pivots (piv: ARRAY[INTEGER]; a: ARRAY[DOUBLE])
        local
           pivotcol: INTEGER
           tmp: BOOLEAN
        do
            across
                0 |..| 119 as i
            loop
                pivotcol := -1
                tmp := true
                across
                    0 |..| 119 as j
                loop
                    if tmp and abs(get(a, i.item, j.item)) > 0.000001 then
                        pivotcol := j.item
                        tmp := false
                    end
                end
                piv.put(pivotcol, i.item)
            end
        end

feature

    abs (a: DOUBLE): DOUBLE
        do
            if a > 0 then
                Result := a
            else
                Result := - a
            end
        end

feature

    mod (a, b: INTEGER): INTEGER
        do
            Result := (a \\ b + b) \\ b
        end

feature

    get (arr: ARRAY [DOUBLE]; r, c: INTEGER): DOUBLE
        do
            Result := arr.at(120 * r + c)
        end

feature

    set (arr: ARRAY [DOUBLE]; r, c: INTEGER; d: DOUBLE)
        do
            arr.put(d, 120 * r + c)
        end

feature

    matmul (output, a, b: ARRAY [DOUBLE])
        local
            acc: DOUBLE
        do
            across
                0 |..| 119 as i
            loop
                across
                   0 |..| 119 as k
                loop
                    acc := 0
                    across
                        0 |..| 119 as j
                    loop
                        acc := acc + get(a, i.item, j.item) * get(b, j.item, k.item)
                    end
                    set(output, i.item, k.item, acc)
                end
            end
        end

end
