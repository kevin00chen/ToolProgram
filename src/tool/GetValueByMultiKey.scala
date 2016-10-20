package tool

import scala.collection.mutable.ListBuffer
import scala.util.parsing.json.JSON

/**
 * Created by ckm on 2016/10/17.
 */
object GetValueByMultiKey {
  def main(args: Array[String]) {
    val json = "{\"glossary\": {\"title\": \"example glossary\",\"GlossDiv\": {\"title\": \"S\",\"GlossList\": {\"GlossEntry\": {\"ID\": \"SGML\",\"SortAs\": \"SGML\",\"GlossTerm\": \"Standard Generalized Markup Language\",\"Acronym\": \"SGML\",\"Abbrev\": \"ISO 8879:1986\",\"GlossDef\": {\"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",\"GlossSeeAlso\": [\"GML\",\"XML\"]},\"GlossSee\": \"markup\"}}}}}"
    val map = JSON.parseFull(json) match {
      case Some(map: Map[_, _]) => map.asInstanceOf[Map[String, Any]]
      case _ => throw new RuntimeException("Json is not correct!")
    }


    def getValueByMutiKey(map: Map[String, Any], keys: String): AnyRef = {
      val keyList = keys.split("\\.")


      def getValueFromMap(map: Map[String, Any], keyList:Array[String]): AnyRef = {
        map.get(keyList.head) match {
          case Some(value: Map[_, _]) if keyList.length == 1 => value
          case Some(value: Map[_, _]) => getValueFromMap(value.asInstanceOf[Map[String, Any]], keyList.drop(1))
          case Some(valueList: List[_]) if keyList.length == 1 => valueList
          case Some(valueList: List[_]) => {
            val list = new ListBuffer[AnyRef]()
            for (value <- valueList.asInstanceOf[List[Map[String, Any]]]) list += (getValueFromMap(value, keyList.drop(1)))
            list.toList
          }
          case Some(value: String) => value
          case None => ""
          case _ => throw new RuntimeException("Json can't parse")
        }
      }

      getValueFromMap(map, keyList)
    }



    val getMapValue = getValueByMutiKey(map, "glossary")
    val getStringValue1 = getValueByMutiKey(map, "glossary.GlossDiv.title")
    val getStringValue2 = getValueByMutiKey(map, "glossary.GlossDiv.GlossList.GlossEntry.GlossTerm")
    val getListValue = getValueByMutiKey(map, "glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso")


    println(getMapValue)
    println(getStringValue1)
    println(getStringValue2)
    println(getListValue)

  }
}
