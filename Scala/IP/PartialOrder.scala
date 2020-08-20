trait PartialOrder[T] {
def <=(that: T): Boolean // checks this <= that. Partial order on T.
def lub(that: T): T // returns the least upper bound of this and that.
}
