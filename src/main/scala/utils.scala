import scala.collection.mutable._
object utils {


  //P05
  def reverseList(alist: List[Int]): List[Int] = {
    var blist = new ListBuffer[Int]
    var size = alist.length - 1
    for (i <- size to 0 by -1) {
      //  print(i)
      blist += alist(i)
    }
    print(blist.toList)
    blist.toList
  }

  //P06
  def palindromeCheck(list: List[Int]): Boolean = {
    var size = list.length
    if (size % 2 != 0) {
      val (a, b) = list.splitAt(size / 2)
      println("A=" + a + "  B=" + b)
      val b1 = b.drop(1)
      //print(b1)
      if (a == reverseList(b1)) {
        println("List is Palindrome !")
        true
      } else {
        println("List isn't Palindrome !")
        false
      }
    }
    else {
      println("List isn't palindrome")
      true
    }
  }

  //P08
  def compresList(list: List[Symbol]): List[Symbol] = {
    var listB = new ListBuffer[Symbol]
    var size = list.length - 1
    for (i <- 0 until size) {
      if (!listB.contains(list(i))) listB += list(i)
    }
    println(listB.toList)
    listB.toList
  }

  //p09
  /* def packList(list: List[Symbol]):List[List[Symbol] = {
    var k
    var listFinal = List[Symbol]
    var listB = new ListBuffer[Symbol]
    for (i <- 0 to list.length - 1) {

      for (j <- 1 to list.length - 1 if list(i) == list(j)) {
        listB += list(j)
      }
    }
  }*/

  //P14
  def duplicateList(list: List[Symbol]): List[Symbol] = {
    var listBuffer = new ListBuffer[Symbol]
    for (i <- 0 to list.length - 1) {
      listBuffer += list(i)
      listBuffer += list(i)
    }
    println(listBuffer.toList)
    listBuffer.toList
  }

  //P15
  def duplicateList(cnt: Int, list: List[Symbol]): List[Symbol] = {
    var listBuffer = new ListBuffer[Symbol]
    for (i <- 0 to list.length - 1) {
      for (j <- 1 to cnt)
        listBuffer += list(i)
    }
    println(listBuffer.toList)
    listBuffer.toList
  }

  //P16
  def dropList(num: Int, list: List[Symbol]): List[Symbol] = {
    var cnt = 1
    var listBuffer = new ListBuffer[Symbol]
    for (i <- 0 to list.length - 1) {
      if (cnt != num) {
        listBuffer += list(i)
        cnt += 1
      } else {
        cnt = 1
      }
    }
    println(listBuffer.toList)
    listBuffer.toList
  }


  //P17
  def splitList(index: Int, list: List[Symbol]): (List[Symbol], List[Symbol]) = {
    var listBuffer1 = new ListBuffer[Symbol]
    var listBuffer2 = new ListBuffer[Symbol]
    for (i <- 0 to list.length - 1) {
      if (i < index) listBuffer1 += list(i)
      else listBuffer2 += list(i)
    }
    println((listBuffer1.toList, listBuffer2.toList))
    (listBuffer1.toList, listBuffer2.toList)
  }

  //P18
  def sliceList(startIndex: Int, endIndex: Int, list: List[Symbol]): List[Symbol] = {
    var listBuffer = new ListBuffer[Symbol]
    // var listBuffer2 = new ListBuffer[Symbol]
    for (i <- startIndex to endIndex) {
      listBuffer += list(i)
    }
    println(listBuffer.toList)
    (listBuffer.toList)
  }

  //P19


  //P20
  def removeAtList(Index: Int, list: List[Symbol]): (List[Symbol], Symbol) = {
    var listBuffer = new ListBuffer[Symbol]
    for (i <- 0 to list.length - 1 if i != Index) {
      listBuffer += list(i)
    }
    println((listBuffer.toList, list(Index)))
    (listBuffer.toList, list(Index))
  }

  def removeAtList2(Index: Int, list: List[Int]): (List[Int], Int) = {
    var listBuffer = new ListBuffer[Int]
    for (i <- 0 to list.length - 1 if i != Index) {
      listBuffer += list(i)
    }
    println((listBuffer.toList, list(Index)))
    (listBuffer.toList, list(Index))
  }

  //P21
  def insertAtList(sym:Symbol,Index: Int, list: List[Symbol]): List[Symbol] = {
    var listBuffer = new ListBuffer[Symbol]
    if (Index > list.length) {
      for (i <- 0 to Index) {
        if (i == Index)
          listBuffer += sym
        else
          listBuffer += list(i)
      }
      for (i <- Index to list.length - 1)
        listBuffer += list(i)
      println(listBuffer.toList)
      listBuffer.toList
    } else {
      for (i <- 0 to list.length - 1)
        listBuffer += list(i)
      listBuffer += sym
      println(listBuffer.toList)
      listBuffer.toList
    }
  }

  //P22
  def rangeList(startVal:Int,endVal:Int):List[Int]={
    var listBuffer = new ListBuffer[Int]
    var value = startVal
    if(endVal > startVal )
    {
      for(i <- 1 to (endVal-startVal+1))
      {
        listBuffer+= value
        value+=1
      }
      println(listBuffer.toList)
      listBuffer.toList
    }
    else println("End Value cann't be less than start value !");listBuffer.toList
  }

  //P23

  def randomSelectList(cnt:Int,list: List[Symbol]):List[Symbol]={
    var listBuffer=new ListBuffer[Symbol]
    //var(a:List[Symbol],b:Symbol)=(list,'x)
    var temp =(list,'x)
    //print((a,b))
    var ind = 0
    //var (c:List[Symbol],d:Symbol)
    import scala.util.Random
    for(i<-1 to cnt)
      {
       // println(removeAtList(i,list))
        if(temp._1.length-1 != 0)
         {ind = Random.nextInt(temp._1.length-1)
         temp= removeAtList(ind,temp._1)
        listBuffer += temp._2
         } else {
          temp= removeAtList(0,temp._1)
          listBuffer += temp._2
        }

      }
    println("Random Select List => "+listBuffer.toList)
    listBuffer.toList
  }

  //P24
  def lotto(cnt:Int,endList:Int):List[Int]={
    var listBuffer=new ListBuffer[Int]
    var temp = (rangeList(1,endList),3)
    //print((a,b))
    var ind = 0
    //var (c:List[Symbol],d:Symbol)
    import scala.util.Random
    for(i<-1 to cnt)
    {
      // println(removeAtList(i,list))
      ind = Random.nextInt(temp._1.length-1)
      temp= removeAtList2(ind,temp._1)
      listBuffer += temp._2
    }
    println(listBuffer.toList)
    listBuffer.toList
  }

  //P25
  def randomPermuteList(list:List[Symbol]):List[Symbol]={
    var size = list.length
    val flist= randomSelectList(size,list)
    println(flist)
    flist
  }
}