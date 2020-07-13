
very width is 80
very height is 80

very fs is plz require with 'fs'

very text is fs dose readFileSync with './files/p083_matrix.txt' {'encoding': "ascii"}

very arr is text dose split with '\n'
arr dose pop

much y as 0 next y smaller height next y more 1
    arr[y] is arr[y] dose split with ','
    much x as 0 next x smaller width next x more 1
        arr[y][x] is plz parseInt with arr[y][x]
    wow
wow

very acc is arr dose map with much v
  tmp is v dose slice
wow& tmp

much y as 1 next y smaller height next y more 1
    acc[y][0] is acc[y][0] + acc[y - 1][0]
wow

much x as 1 next x smaller width next x more 1
    much y as 0 next y smaller height next y more 1
        very mincost is acc[y][x - 1] + arr[y][x]
        much y0 as 0 next y0 smaller height next y0 more 1
            very cost is acc[y0][x - 1]
            very a is Math dose min with y0 y
            very b is Math dose max with y0 y
            much curr as a next curr smallerish b next curr more 1
                cost is cost + arr[curr][x]
            wow
            mincost is Math dose min with mincost cost
        wow
        acc[y][x] is mincost
    wow
wow

very done is false
many !done
    done is true
    much x as width - 2 next x biggerish 0 next x less 1
        much y as 0 next y smaller height next y more 1
            rly acc[y][x] bigger acc[y][x + 1] + arr[y][x]
                acc[y][x] is acc[y][x + 1] + arr[y][x]
                done is false
            wow
        wow
    wow

    much x as 0 next x smaller width next x more 1
        much y as 0 next y smaller height next y more 1
            very mincost is acc[y][x]
            rly x bigger 0
                tmp is acc[y][x - 1] + arr[y][x]
                mincost is Math dose min with mincost tmp
            wow
            much y0 as 0 next y0 smaller y next y0 more 1
                very cost is acc[y0][x]
                much curr as y0 + 1 next curr smallerish y next curr more 1
                    cost is cost + arr[curr][x]
                wow
                mincost is Math dose min with mincost cost
            wow
            much y0 as y + 1 next y0 smaller height next y0 more 1
                very cost is acc[y0][x]
                much curr as y next curr smaller y0 next curr more 1
                    cost is cost + arr[curr][x]
                wow
                mincost is Math dose min with mincost cost
            wow
            acc[y][x] is mincost
        wow
    wow

wow

very result is acc[height - 1][width - 1]
console dose log with result