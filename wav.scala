import scala.io.Source

object wav extends App {
  
	for(line <- Source.fromFile("E:\\Projects\\tabs\\output-args.txt").getLines())
	{
	    val chord: Array[String] = line.split(" ");
		println(chord.reduceLeft((x1: String, x2: String) => x1 + " " + x2))
		val joined = chord.reduceLeft((x1: String, x2: String) => x1 + "-" + x2)
	    val filename = "E:\\Projects\\tabs\\testfiles\\" + joined + ".wav";
	    
		pitches.write(filename, chord)
		
		pitches.read(filename, chord.length, chord)
	}
}