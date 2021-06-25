package base

import org.apache.commons.io.FileUtils

import java.io.File
import scala.io.Codec
import scala.util.Try

trait FileSpec { self: TestBaseDefinition =>

  def readTestResourceFile(filename: String) =
    Try(new File(s"test/res/$filename"))
      .map(f => FileUtils.readFileToString(f, Codec.UTF8.charSet))
      .success
      .value
}
