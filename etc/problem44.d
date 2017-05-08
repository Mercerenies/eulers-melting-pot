
import std.stdio;
import std.container;
import std.conv;
import std.math;
import std.algorithm;

struct unit {}

int pent(int n) {
    return n * (3 * n - 1) / 2;
}

bool is_pent(int n) {
    return sqrt(to!real(1 + 24 * n)) % 6 == 5; // Long story
}

struct Node {

    int x0, x1;

    this(int a, int b) {
        x0 = a;
        x1 = b;
    }

    int cost() {
        return abs(pent(x1) - pent(x0));
    }

    bool matches() {
        return
            is_pent(pent(x1) - pent(x0)) &&
            is_pent(pent(x1) + pent(x0));
    }

    Node[] expand() {
        return [
                Node(this.x0 + 1, this.x1 + 1),
                Node(this.x0, this.x1 + 1)
                ];
    }

}
/*
void main() {
    unit[Node] frontier;
    frontier[Node(1, 2)] = unit();
    while (true) {
        Node mini = frontier.byKey().minElement!"a.cost()";
        frontier.remove(mini);
        if (mini.matches()) {
            writeln(to!string(mini.cost()));
            break;
        }
        foreach (x; mini.expand()) {
            frontier[x] = unit();
        }
        writeln(" * " ~ to!string(mini.cost()));
    }
}
void main() {
    auto frontier = heapify!"a.cost() > b.cost()"(Array!Node(Node(1, 2)));
    while (true) {
        Node mini = frontier.front();
        frontier.removeFront();
        if (mini.matches()) {
            writeln(to!string(mini.cost()));
            break;
        }
        foreach (x; mini.expand()) {
            frontier.insert(x);
        }
        writeln(" * " ~ to!string(mini.cost()));
    }
}
*/
void main() {
    for (auto i = 1;; i++) {
        for (auto j = i - 1; j > 0; j--) {
            writeln(to!string(i));
            if (is_pent(pent(i) + pent(j)) && is_pent(pent(i) - pent(j))) {
                writeln(to!string(pent(i) - pent(j)));
                return;
            }
        }
    }
}

