[indent=4]

init
    var result = solve_all()
    for value in result.data
        stdout.printf("%d", (int)value)
    print("")

interface BinaryOp:Object
    def abstract apply(a : float, b : float) : float

class Add:Object implements BinaryOp
    def apply(a : float, b : float) : float
        return a + b

class Sub:Object implements BinaryOp
    def apply(a : float, b : float) : float
        return a - b

class Mul:Object implements BinaryOp
    def apply(a : float, b : float) : float
        return a * b

class Div:Object implements BinaryOp
    def apply(a : float, b : float) : float
        return a / b

interface TreeNode:Object

    def abstract apply(args : GenericArray of (float?), index : int) : float

    def abstract get_weight() : int

class LeafNode:Object implements TreeNode

    def apply(args : GenericArray of (float?), index : int) : float
        return args.@get(index)

    def get_weight() : int
        return 1

class BranchNode:Object implements TreeNode
    left: TreeNode
    right: TreeNode
    op: BinaryOp

    construct(left_ : TreeNode, right_ : TreeNode, op_ : BinaryOp)
        left = left_
        right = right_
        op = op_

    def get_weight() : int
        return left.get_weight() + right.get_weight()

    def apply(args : GenericArray of (float?), index : int) : float
        var pivot = index + left.get_weight()
        var lhs = left.apply(args, index)
        var rhs = right.apply(args, pivot)
        return op.apply(lhs, rhs)


def all_trees(n : int, arr : GenericArray of TreeNode)
    var ops = new GenericArray of BinaryOp()
    ops.add(new Add())
    ops.add(new Sub())
    ops.add(new Mul())
    ops.add(new Div())
    if n == 1
        arr.add(new LeafNode())
    else
        var i = 1
        while i < n
            var j = n - i
            var left = new GenericArray of TreeNode()
            var right = new GenericArray of TreeNode()
            all_trees(i, left)
            all_trees(j, right)
            for lhs in left.data
                for rhs in right.data
                    for op in ops.data
                        arr.add(new BranchNode(lhs, rhs, op))
            i += 1

interface SolveCallback:Object
    def abstract apply(args: GenericArray of (float?))

class SolveHashCallback:Object implements SolveCallback
    trees : GenericArray of (TreeNode)
    values : HashTable of (int?, int)

    construct(trees_ : GenericArray of (TreeNode))
        trees = trees_
        values = new HashTable of (int?, int)(int_hash, int_equal)

    def get_values() : HashTable of (int?, int)
        return values

    def apply(args: GenericArray of (float?))
        for tree in trees.data
            var result = tree.apply(args, 0)
            if result == (int)result
                values.insert((int)result, 1)

// Thanks, Wikipedia!
//
// https://en.wikipedia.org/wiki/Heap's_algorithm
def permute(k : int, args : GenericArray of (float?), callback : SolveCallback)
    if k == 1
        callback.apply(args)
    else
        permute(k - 1, args, callback)
        var i = 0
        while i < k - 1
            if k % 2 == 0
                var tmp = args.@get(i)
                args.@set(i, args.@get(k - 1))
                args.@set(k - 1, tmp)
            else
                var tmp = args.@get(0)
                args.@set(0, args.@get(k - 1))
                args.@set(k - 1, tmp)
            permute(k - 1, args, callback)
            i += 1

def solve_values(trees : GenericArray of (TreeNode), args : GenericArray of (float?)) : int
    var callback = new SolveHashCallback(trees)
    permute(args.length, args, callback)
    var values = callback.get_values()
    var i = 1
    while true
        if !values.contains(i)
            return i
        i += 1

def sort_cmp(a : float?, b : float?) : int
    return (int)(a - b)

def solve_all() : GenericArray of (float?)
    var best = 0
    var best_values = new GenericArray of (float?)()
    var trees = new GenericArray of TreeNode()
    all_trees(4, trees)
    var a = 1
    while a <= 6
        var b = a + 1
        while b <= 7
            var c = b + 1
            while c <= 8
                var d = c + 1
                while d <= 9
                    var args = new GenericArray of (float?)()
                    args.add(a)
                    args.add(b)
                    args.add(c)
                    args.add(d)
                    var curr = solve_values(trees, args)
                    if curr > best
                        best = curr
                        best_values = args
                    d += 1
                c += 1
            b += 1
        a += 1
    best_values.sort(sort_cmp)
    return best_values
