trait Tree[T]:
    def size:Int
    def add(t:T):Tree[T]
    def eq[T](a: T, b: T, leq: (T, T) => Boolean) = leq(a,b) && leq(b,a)
    def le[T](a: T, b: T, leq: (T, T) => Boolean) = !leq(b,a)
    def toList:List[T]




case class EmptyTree[T](leq: (T, T) => Boolean) extends Tree[T]:
    def size: Int = 0
    def add(t:T):Tree[T] = new Node(EmptyTree(leq), t, EmptyTree(leq), leq)
    def toList:List[T] = Nil

case class Node[T](left: Tree[T], elem: T, right: Tree[T], leq: (T, T) => Boolean) extends Tree[T]:
    def size:Int = left.size + right.size +1
    def add(t:T):Tree[T] =
        if eq(t, elem, leq) then this 
        else if le(t,elem, leq) then Node(left.add(t), elem, right, leq)
        else Node(left, elem, right.add(t), leq)
    def toList:List[T] =
       val ls = elem :: right.toList
       return left.toList ::: ls

import util.Random.nextInt



    

val intLeq: (Int, Int) => Boolean = (x, y) => x <= y
val emptyIntTree: Tree[Int] = new EmptyTree(intLeq)

// Question 1:
/*
eq(2, 2, intLeq)
eq(2, 3, intLeq) == false

le(1, 2, intLeq)
le(2, 2, intLeq) == false
le(3, 2, intLeq) == false*/


// Question 2
assert(EmptyTree(intLeq).size == 0)
assert(Node(EmptyTree(intLeq), 1, EmptyTree(intLeq), intLeq).size == 1)

val exampleTree = Node(
  EmptyTree(intLeq),
  1,
  Node(EmptyTree(intLeq), 2, EmptyTree(intLeq), intLeq),
  intLeq
)
assert(exampleTree.size == 2)

//Question 4
def generate_leaf(x: Int, leq: (Int, Int) => Boolean = intLeq) = Node(EmptyTree(leq), x, EmptyTree(leq), leq)

assert(EmptyTree(intLeq).add(5) == generate_leaf(5))

// Adding a number that is already present should not change the tree
assert(EmptyTree(intLeq).add(5).add(5) == generate_leaf(5))

// Adding a smaller number should put it at the left
assert(EmptyTree(intLeq).add(5).add(4) == Node(generate_leaf(4), 5, EmptyTree(intLeq), intLeq))

// Adding a greater number should put it at the right
assert(EmptyTree(intLeq).add(5).add(6) == Node(EmptyTree(intLeq), 5, generate_leaf(6), intLeq))

// Flipping the `leq` function should make bigger elements at the left
val geq: (Int, Int) => Boolean = (x, y) => x >= y
assert(EmptyTree(geq).add(5).add(6).add(4) == Node(generate_leaf(6, geq), 5, generate_leaf(4, geq), geq))

def sortedList[T](leq: (T, T) => Boolean, ls: List[T]): List[T] = 
    def loop(ls:List[T], tr:Tree[T]):Tree[T] = 
        if ls.isEmpty then tr
        else loop(ls.tail, tr.add(ls.head))
    val tr:Tree[T] =  loop(ls, EmptyTree(leq))
    tr.toList


import util.Random.nextInt

// Generate a random list of 10 elements
val randomList = (0 until 10).map(n => nextInt).toList
// Adds every element of the list to the tree
val tree = randomList.foldLeft[Tree[Int]](EmptyTree(intLeq))((tree, element) => tree.add(element))

randomList.sorted == sortedList(intLeq, randomList)