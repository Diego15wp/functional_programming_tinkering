import scala.annotation.tailrec

class ListNode(_x: Int, _next: ListNode = null){
  val x = _x
  var next = _next
}

object ReverseLinkedList {

  def go = {
    val v1 = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))
    println(s"The original list: ${viewList(v1)}")
    val r_imperative = reverseListImperative(v1)
    println(s"The reversed list (imperative): ${viewList(r_imperative)}")
    val v2 = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))
    println(s"The original list again: ${viewList(v2)}")
    val r_functional = reverseListFunctional(v2)
    println(s"The reversed list (functional): ${viewList(r_functional)}")
  }

  def viewList(head: ListNode): String = {
    def view(node: ListNode): String = 
      if (node == null) "(null)" else s"(${node.x})-->>" + view(node.next)
    view(head)
  }

  def reverseListImperative(head: ListNode): ListNode = {
    var iter = head // functions parameters are immutable so copy of head made into var
    var prev:ListNode = null //reverse list will point to null

    //loop that ends when no more nexts in list
    while(iter != null){
      //swap prev with next
      var swapper = iter.next
      iter.next = prev
      prev = swapper

      //update iterator and prev for next loop
      swapper = iter
      iter = prev
      prev = swapper
    }
    //return final ListNode
    prev
  }

  def reverseListFunctional(head: ListNode): ListNode = {

    @annotation.tailrec
    def go(prevNode: ListNode, currNode:ListNode): ListNode = {
      if(currNode  == null){prevNode}   //if curr is null end of original list reached
      else{
        val newNode: ListNode = currNode.next //temp value for original list order
        currNode.next = prevNode  //update next to previous
        go(currNode, newNode)   //recursive call to next reversal pair
      }
    }
    go(null, head)
  }

}
