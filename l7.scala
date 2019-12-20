import scala.io.Source
object scalawordcount {
  def main(args:Array[String]){
    if(args.length!=1){
      System.err.println("Usage: sbt run \"filepath\"")
      System.exit(0)
    }
    var fn = args(0)
    var wc = scala.collection.mutable.Map[String,Int]()
    for(line<-Source.fromFile(fn).getLines)
      for(word<-line.split(" "))
        wc(word) = if(wc.contains(word))
                wc(word)+1
              else 1
    for((k,v)<-wc)
      printf("Word %s occurs %d time\n",k,v)
      println(wc)
  }
}