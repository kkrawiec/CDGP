package sygusgp

import scala.sys.process.ProcessIO
import java.io.InputStream
import java.io.OutputStream
import java.util.Scanner
import scala.sys.process.Process

object TestProcessIO extends App {
  def feedInput(in: OutputStream): Unit = {
    for (ln <- io.Source.stdin.getLines) {
      print("> ")
      in.write((ln + "\n").toCharArray().map(_.toByte))
      in.flush // very important
    }
  }
  def processOutput(pref: String)(os: InputStream): Unit = {
    val sc = new Scanner(os)
    while (true) {
      println(pref + sc.nextLine)
    }
  }
  val pb = Process("""/bin/bash""")
  val pio = new ProcessIO(feedInput,
    processOutput("OUT:"), processOutput("ERR:"))
  pb.run(pio) // don't wait
}
