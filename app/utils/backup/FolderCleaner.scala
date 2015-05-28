package utils.backup

import java.io.{FileOutputStream, File}

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat


class FolderCleaner {

  val dateFormat = "yyyyMMdd_HHmmss"

  def clean(path: String, days: Int): Unit = {

    val theDir: File = new File(path)
    if (!theDir.exists()) {
      theDir.mkdir()
    } else if (theDir.list().length > 0) {
      for (fileName <- theDir.list()) {
        deleteIfOlderThan(days, fileName, path)
      }
    }

  }

  private def deleteIfOlderThan(days: Int, fileStr: String, path: String): Unit = {

    val filePath = path + fileStr

    val dateSubString = fileStr.substring(0, 15)

    val lowerBoundDate = DateTime.now.minusDays(days)

    val fileDate = dateFromString(dateSubString)

    val file: File = new File(filePath)

    if (fileDate.isBefore(lowerBoundDate)) {
      file.delete()
    }

  }

  def stringFromDate(date: DateTime): String = {

    date.toString(dateFormat)

  }

  def dateFromString(str: String): DateTime = {

    val dateFmt = DateTimeFormat.forPattern(dateFormat)

    DateTime.parse(str, dateFmt)

  }

}

