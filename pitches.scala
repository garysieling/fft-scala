import java.io.File
import java.lang.Math
import org.apache.commons.math3.transform.DftNormalization
import org.apache.commons.math3.transform.FastFourierTransformer
import org.apache.commons.math3.transform.TransformType
import org.apache.commons.math3.complex.Complex
import scala.collection.mutable.Map
import scala.util.Sorting
import scala.collection.SortedMap


object pitches {
	def write(filename: String, chord: Array[String]) {
		
		val frequencies = Map(
				("A", 220.00),
				("A#", 233.08),
				("B", 246.94),
				("C", 261.63),
				("C#", 277.18),
				("D", 293.66),
				("D#", 311.13),
				("E", 329.63),
				("F", 349.23),
				("F#", 369.99),
				("G", 392.00),
				("G#", 415.30))
		
		val notes : Array[Double] = Array.fill(chord.length)(0.0);
		
		var noteIndex: Integer = 0;
		for (note : String <- chord)
		{
		   notes(noteIndex) = frequencies(note);
		   noteIndex = noteIndex + 1
		}
		
	  	val file = new File(filename)
		
		val sampleRate : Long = 44100; 
	    val duration : Double = 1.0;
	    val bufferSize : Int = 100;
	
	    val numFrames : Long = (duration * sampleRate).toLong
	    val wavFile = WavFile.newWavFile(file, 1, numFrames, 16, sampleRate);
	
	    var frameCounter : Int = 0;
		
		val SAMPLE_RATE = 8000f;
		val msecs = 1000.0;
		
	    while (frameCounter < numFrames)
	    {
	    	val buffer : Array[Int] = Array.fill(bufferSize)(0);
	      // Determine how many frames to write, up to a maximum of the buffer size
	        val remaining : Long = wavFile.getFramesRemaining();
	        val toWrite = if (remaining > bufferSize) bufferSize else remaining.toInt;
	
	        // Fill the buffer, one tone per channel
	        for (val s : Int <- 0 to toWrite - 1)
	        {
	           frameCounter = frameCounter + 1
	           for (j : Int <- 0 to (notes.length - 1))
	           {
	        	   buffer(s) = buffer(s) + (1000 * Math.sin(2.0 * Math.PI * notes(j) * frameCounter / sampleRate)).toInt;
	           }
	        }
	
	        wavFile.writeFrames(buffer, toWrite)
	    }

	    wavFile.close()
	}
	
	def read(filename: String, numNotes: Integer, expected : Array[String])
	{
	    val notes = Map(
				("C", 16.35),
				("C#", 17.32),
				("D", 18.35),
				("D#", 19.45),
				("E", 20.60),
				("F", 21.83),
				("F#", 23.12),
				("G", 24.50),
				("G#", 25.96),
				("A", 27.50),
				("A#", 29.14),
				("B", 30.87))

		val tones = notes map { _.swap }
	    
		val wavFile = WavFile.openWavFile(new File(filename))
	    
	    val sampleRate : Long = 44100
        val numChannels : Integer = wavFile.getNumChannels()
	    var sampleSize : Integer = 1024 * 16 
	    
	    val dataSize : Int = sampleSize * numChannels 
	    val frameData : Array[Double] = Array.fill(dataSize)(0.0)
	    
		wavFile.readFrames(frameData, dataSize)
	    
		val fft : FastFourierTransformer = new FastFourierTransformer(
				DftNormalization.UNITARY)

		val results : Array[Complex] = fft.transform(frameData, TransformType.FORWARD);
		var index = 0
		var foundNotes : Map[String, Double] = Map();
		for (val c : String <- notes.keys)
		{
		   foundNotes(c) = 0.0
		}
		
		for (val c : Complex <- results)
		{
		    val magnitude : Double = 
		    		Math.sqrt(c.getImaginary() * c.getImaginary() + 
		    				  c.getReal() * c.getReal())
		     
	        val frq = (.5 + index) * sampleRate / dataSize; 
	        if (0 < frq && frq < sampleRate / 2 - 1)
	        {
	        	var noteFound = "";
	        	var foundFrq = 0.0;
	        	var distance = Double.MaxValue
	        	for ((note, noteFrq) <- notes) {
	        	    var num = Math.log(frq/noteFrq)/Math.log(2)
	        	    var normFrq = Math.abs(num - Math.floor(num));
	        		if (normFrq < distance)
	        		{
	        		  distance = normFrq
	        		  noteFound = note
	        		  foundFrq = normFrq
	        		}
	        	}
	        	
	        	if (noteFound != "")
	        	{
	        		foundNotes(noteFound) = foundNotes(noteFound) + magnitude
	        	}
	        }
		    
		    index = index + 1
		}
		
		var foundTones : Map[Double, String] = foundNotes map { _.swap }
		var magnitudes : List[Double] = foundTones.keys.toList
		var idx : Integer = 0		
		
		var resultsSorted = foundTones.toList sortBy {_._1}
		
		var authAnswer = resultsSorted.slice(resultsSorted.length - expected.length, resultsSorted.length);

		for (val testNote <- expected)
		{
			var foundNote = false
			for (val authNote <- authAnswer)
		    {
				if (authNote._2 == testNote)
				{
				   foundNote = true
				}
		    }
			
			if (!foundNote)
			{
			    println("fail (" + testNote + ")")
			    return
			}
		}
		
		println("success")
	}
}