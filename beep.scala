package beep

import scala.concurrent.ops._
import javax.sound.sampled._

object main {
	def main(args:Array[String]) {
		var beep = new Beep
		beep.play(0,32,32);
	}
}
class BeepData(var phase:int,var pitch:int, var count:int, var sweep:int) 
class Beep {
	def play(pitch:int, count:int, sweep:int) { _beeps(_beepPos)=new BeepData(0,pitch,count,sweep); _beepPos = (_beepPos + 1) & 7 }
	def stop { _stop = 2 }
	private var _stop, _beepPos = 0
	private var _beeps = new Array[BeepData](8)
	private val _wave = new Array[Byte](16); _wave(1) = 40; _wave(2) = -40
	private val _data = new Array[Byte](256)
	private val _data1 = new Array[Int](256)
	private def stream {
		for (i <- 0 until 256)_data1(i)=0
		for (j <- 0 until _beeps.length if(_beeps(j)!=null)) {
			var b = _beeps(j)
			for (i <- 0 until 256) { b.phase = (b.phase + (b.pitch & 255)) & 4095; _data1(i) = (_data1(i) + _wave(b.phase >> 8)) }
			if (b.count == 0) _beeps(j)=null else { b.count -= 1; b.pitch += b.sweep }
		}
		for (i <- 0 until 256){
			var d = _data1(i)
			d=(if(d==0)0.0 else if(d > 0) Math.log(d)*21 else Math.log(-d)* -21).asInstanceOf[Int]
			if(d > 127) d = 127 else if(d < -127) d = -127
			_data(i)=d.asInstanceOf[Byte]
		}
	}
	val fmt = new AudioFormat(AudioFormat.Encoding.PCM_SIGNED , 11025, 8, 1, 1, 11025, true)
	val line = AudioSystem.getLine(new DataLine.Info(classOf[SourceDataLine], fmt)).asInstanceOf[SourceDataLine]
	line.open(); line.start; line.write(_data, 0, 256)
	spawn {
		Thread.currentThread.setPriority(10)
		while (_stop < 2) {
			line.write(_data, 0, 256); stream
			var size = line.getBufferSize - line.available
			if (size > 256) Thread.sleep(1000*(size-256)/11025)
		}
		line.flush; line.stop; line.close
	}
}

/*
class Beep {
	def play(pitch:int, count:int, sweep:int) { _pitch = pitch; _count = count; _sweep = sweep }
	def stop { _stop = 2 }
	private var _phase, _pitch, _count, _sweep, _stop = 0
	private val _wave = new Array[Byte](16); _wave(1) = 120; _wave(2) = -120
	private val _data = new Array[Byte](256)
	private def stream {
		for (i <- 0 until 256) { _phase = (_phase + (_pitch & 255)) & 4095; _data(i) = _wave(_phase >> 8) }
		if (_count == 0) _pitch = 0 else { _count -= 1; _pitch += _sweep }
	}
	val fmt = new AudioFormat(AudioFormat.Encoding.PCM_SIGNED , 11025, 8, 1, 1, 11025, true)
	val line = AudioSystem.getLine(new DataLine.Info(classOf[SourceDataLine], fmt)).asInstanceOf[SourceDataLine]
	line.open(); line.start; line.write(_data, 0, 256)
	spawn {
		while (_stop < 2) {
			line.write(_data, 0, 256); stream
			var size = line.getBufferSize - line.available
			if (size > 256) Thread.sleep(1000*(size-256)/11025)
		}
		line.flush; line.stop; line.close
	}
}
*/
