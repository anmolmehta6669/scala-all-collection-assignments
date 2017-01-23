package CollectionAssignment

/**
  * Created by Anil Mehta on 23-01-2017.
  */
object StudentMarks {
  def main(args: Array[String]): Unit = {
    val studentsList = List(StudentCase(1, "Anmol", Gender.male), StudentCase(2, "Archana", Gender.female), StudentCase(3, "Saniya", Gender.female),
      StudentCase(4, "Babbar", Gender.male), StudentCase(5, "Dolly", Gender.female), StudentCase(6, "Jassi", Gender.female), StudentCase(7, "Shubham", Gender.male),
      StudentCase(8, "Anuja", Gender.female), StudentCase(9, "Prerna", Gender.female), StudentCase(10, "Shubham", Gender.male))
    val marksList = List(MarksCase(1, 1, 100), MarksCase(1, 2, 100), MarksCase(1, 3, 67), MarksCase(1, 4, 70), MarksCase(1, 5, 80),
      MarksCase(1, 6, 40), MarksCase(1, 7, 76), MarksCase(1, 8, 70), MarksCase(1, 9, 96), MarksCase(1, 10, 56),
      MarksCase(2, 1, 90), MarksCase(2, 2, 100), MarksCase(2, 3, 90), MarksCase(2, 4, 95), MarksCase(2, 5, 60),
      MarksCase(2, 6, 80), MarksCase(2, 7, 66), MarksCase(2, 8, 80), MarksCase(2, 9, 76), MarksCase(2, 10, 86),
      MarksCase(3, 1, 85), MarksCase(3, 2, 80), MarksCase(3, 3, 80), MarksCase(3, 4, 80), MarksCase(3, 5, 80),
      MarksCase(3, 6, 80), MarksCase(3, 7, 80), MarksCase(3, 8, 80), MarksCase(3, 9, 80), MarksCase(3, 10, 80),
      MarksCase(4, 1, 60), MarksCase(4, 2, 60), MarksCase(4, 3, 60), MarksCase(4, 4, 60), MarksCase(4, 5, 60),
      MarksCase(4, 6, 60), MarksCase(4, 7, 70), MarksCase(4, 8, 60), MarksCase(4, 9, 60), MarksCase(4, 10, 60),
      MarksCase(5, 1, 90), MarksCase(5, 2, 85), MarksCase(5, 3, 80), MarksCase(5, 4, 90), MarksCase(5, 5, 95),
      MarksCase(5, 6, 50), MarksCase(5, 7, 90), MarksCase(5, 8, 70), MarksCase(5, 9, 74), MarksCase(5, 10, 60)
    )
    //-----------------------------------------------
    val mapList=mapping(marksList,studentsList)
    println("Score Card as a Map having name as key and List of List of scorecards as value (basically done for duplicate names)")
    mapList.map(println _)
    println("Here we are doing Searching by the name Shubham")
    searchByName(marksList,studentsList,"shubham")
    //------------------------------------------------
    val scoreCardByGender=getScoreCardByGender(marksList,studentsList)
    println(s"male: ${scoreCardByGender._1}")
    println(s"female: ${scoreCardByGender._2}")
    //-------------------------------------------------
    val aboveScorers=scoredAbove(marksList,studentsList,80)
    println("The students scoring above 80% are: ")
    aboveScorers._1.map(println _)
    aboveScorers._2.map(println _)
    //-------------------------------------------------
    val similarScorers=similarPercentagesBetweenGenders(marksList,studentsList)
    println("the same scorers of different genders are: ")
    similarScorers.map(println _)
    //--------------------------------------------------
    println("The percentages of female group that are not in male are: ")
    femalePercentageNotInMale(marksList,studentsList).map(println _)
  }

  /**
  Q1. A) Now, I require a case class named ScoreCard having fields (studentId: Long, marks: Map[Long, Float], percentage: Float).

Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard. As there can be more than one student
  with same name, the logic we have to follow is that, if two or more student has same name the key shold be the name of the student
  and the values (ScoreCard s) should be in a List, otherwise the key should be the student name and value should be the case class ScoreCard.
  e.g. Map should be Map[String, AnyRef].
*/


  def mapping(marksList: List[MarksCase],namelist: List[StudentCase]): Map[String,AnyRef] =
  {
    val scoreList = scoreCardGenerator(marksList).map(x => ScoreCard(x._1, x._2, x._3))
    val tempList=for(x<-scoreList;y<-namelist if(x.studentId==y.id)) yield (y.name,x)
    val temp=tempList.groupBy(x=>x._1)
    temp.map(x=> (x._1,x._2.map(y=>y._2)))
  }
  def scoreCardGenerator(marksList: List[MarksCase]): List[(Long, Map[Long, Float], Float)] = {
    {
      val tempPair1 = marksList.groupBy(x => x.studentId) //Key value pair having studentId as key & list of list of MarksCase as value
    val tempMap = tempPair1.map(x => (x._1, (x._2.map(y => (y.subjectId, y.marks))).toMap, x._2.map(y => y.marks).sum / 5))
      tempMap.toList
    }
  }
/**
  Q1. B) Write a method which takes input as student name and print the score cards. If it finds one or more than one score card  print all of them
  other wise print "No data found". The print should be in increasing order of the student id.
  */


  def searchByName(marksList:List[MarksCase],studentsList:List[StudentCase],name:String){
    val map_ScoreCard=mapping(marksList,studentsList)
    val result=for(x<-map_ScoreCard if(x._1.equalsIgnoreCase(name))) yield x._2
    if(!result.isEmpty) result.map(println _)
    else println("Name not found")
  }

  /**
  Q2. The Student class should contain one more field this time, gender. The values of gender must be set in a Enumeration.

  A) Write a method getScoreCardByGender to return a tuple of ScoreCards (e.g. (List[ScoreCard], List[ScoreCard])),
  where first field in the tuple has male student's score card and the second field has female student's score cards.
    */

  def getScoreCardByGender(marksList:List[MarksCase],studentsList:List[StudentCase]):(List[(String,ScoreCard)],List[(String,ScoreCard)])={
    val scoreList = scoreCardGenerator(marksList).map(x => ScoreCard(x._1, x._2, x._3))
    val result1 = for (x <- scoreList; y <- studentsList if ((x.studentId == y.id) && y.gender==Gender.male)) yield (y.name,x) // combining the names and marks
    val result2 = for (x <- scoreList; y <- studentsList if ((x.studentId == y.id) && y.gender==Gender.female)) yield (y.name, x) // combining the names and marks

    (result1,result2)
  }

  /**
    *Q2 B)
    * Write a method which calls the getScoreCardByGender method and gives the result which has more than 50%
    */

  def scoredAbove(marksList:List[MarksCase],studentsList:List[StudentCase],percentage:Float): (List[(String,ScoreCard)],List[(String,ScoreCard)]) ={
    val scoreTuple=getScoreCardByGender(marksList,studentsList)
    val result1=scoreTuple._1.flatMap(x=> if(x._2.percentage>percentage) Some(x) else None)
    val result2=scoreTuple._2.flatMap(y=> if(y._2.percentage>percentage) Some(y) else None)
    (result1,result2)
  }

  /**
    Q2 C)
    Write a method to find out similar percentage between the two groups (male, female). for example Geetika -75, Kunal -75
    */

  def similarPercentagesBetweenGenders(marksList:List[MarksCase],studentsList:List[StudentCase]):List[((String,ScoreCard),(String,ScoreCard))]={
    val scoreTuple=getScoreCardByGender(marksList,studentsList)
    val result=for(x<-scoreTuple._1;y<-scoreTuple._2 if(x._2.percentage==y._2.percentage)) yield (x,y)
    result
  }

  /**
    Q2 D)
    Write a method fo find out the percentage that girls group has scored but no same percentage has got in the boys group.
    e.g. ( Geetika -75, Neha - 73, charmy - 72) - (Kunal -75, Anmol - 73, Nitin - 71) = Charmy-72
    */
  def femalePercentageNotInMale(marksList:List[MarksCase],studentsList:List[StudentCase]):List[(String,ScoreCard)]={
    val scoreTuple=getScoreCardByGender(marksList,studentsList)
    //val result=for(x<-scoreTuple._2;y<-scoreTuple._1 if(x._2.percentage==y._2.percentage)) yield x
    val result=similarPercentagesBetweenGenders(marksList,studentsList)
    scoreTuple._2.diff(result.map(x=>x._2))
  }
}

