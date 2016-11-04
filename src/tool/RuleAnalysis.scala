package tool

/**
 * Created by ckm on 2016/11/4.
 */

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
 * Created by CHENKAIMING379 on 2016-11-01.
 */
case class Elements(id: String, value: Any)

object RuleAnalysis {
  def handleNormalExpr1(subRule: String, datas: List[Elements]): Boolean = {
    try {
      val keyCountMap = mutable.Map.empty[String, Int]
      val keyMessageMap = mutable.Map.empty[String, Elements]
      for (data <- datas) { // 对数据List进行处理
        keyCountMap(data.id) = keyCountMap.getOrElse(data.id, 0) + 1
        keyMessageMap(data.id) = data
      }
      val keys = new Regex("d[0-9]+").findAllIn(subRule).toList.sortBy(-_.length) // 根据d1, d2, d11的长度排序，将d11排在d1前，优先替换掉d11，避免出现错误
      var compareExpr = subRule
      if (keyCountMap.exists(_._2 > 1)) false //如果某个id字段有超过一条记录，核对失败
      else {
        keys.foreach(key => {
          compareExpr = compareExpr.replace(key, keyMessageMap(key.replace("d", "")).value.toString)
        })

        val ruleEle = compareExpr.split("=")
        if (ruleEle.length != 2 || compareExpr.indexOf("=") == 0 || compareExpr.indexOf("=") == compareExpr.length - 1)
          false
        else {
          val bothRuleSides = (ruleEle(0), ruleEle(1))
          val List(leftValue, rightValue) = bothRuleSides.productIterator.map(oneRuleSide => CalcStrExpression.evaluateStr(oneRuleSide.toString)).toList
          leftValue == rightValue
        }
      }
    } catch {
      case e: Exception => false
    }
  }

  def handleRule(subRule: String, dataList: List[Elements]): Boolean = {

    def handleNormalExpr(oneRuleSide: String): (Boolean, Any) = {
      val keyCountMap = mutable.Map.empty[String, Int]
      val keyMessageMap = mutable.Map.empty[String, Elements]
      for (data <- dataList) { // 对数据List进行处理
        keyCountMap(data.id) = keyCountMap.getOrElse(data.id, 0) + 1
        keyMessageMap(data.id) = data
      }

      val keys = new Regex("d[0-9]+").findAllIn(oneRuleSide.toString).toList.sortBy(-_.length)
      var result = true
      var compareExpr = oneRuleSide.toString
      keys.foreach(key => {
        if (keyCountMap(key.replace("d", "")) > 1) result = false
        compareExpr = compareExpr.replace(key, keyMessageMap(key.replace("d", "")).value.toString)
      })
      (result, CalcStrExpression.evaluateStr(compareExpr))
    }

    try {
      val keyMessageMap = mutable.Map.empty[String, ListBuffer[Elements]]
      for (data <- dataList) { // 对数据List进行处理
        keyMessageMap(data.id) = keyMessageMap.getOrElse(data.id, new ListBuffer[Elements]) += data
      }

      val ruleEle = subRule.split("=")
      if (ruleEle.length != 2 || ruleEle.indexOf("=") == 0 || ruleEle.indexOf("=") == ruleEle.length - 1) // 如果规则配置错误，直接为false
        false
      else {
        val bothRuleSides = (ruleEle(0), ruleEle(1))
        val (bothSideResult, List(leftValue, rightValue)) = bothRuleSides.productIterator.map(oneRuleSide => {
          if (oneRuleSide.toString.contains("sum:")) { // 先求左边表达式的值，如果有求和操作
          var compareExpr = oneRuleSide.toString
            val sumKeys = new Regex("sum:d[0-9]+").findAllIn(compareExpr).toList.sortBy(-_.length) // 首先将其中的sum:d处理掉
            sumKeys.foreach(sumKey => {
              val replaceList = keyMessageMap(sumKey.replace("sum:d", ""))
              val sumKeyValue = replaceList.map(replaceE => replaceE.value).mkString("+")
              compareExpr = compareExpr.replace(sumKey, sumKeyValue)
            })
            if (!compareExpr.contains("d")) (true, CalcStrExpression.evaluateStr(compareExpr))
            else handleNormalExpr(compareExpr)
          } else { // 如果没有求和操作
            if (!oneRuleSide.toString.contains("d")) (true, CalcStrExpression.evaluateStr(oneRuleSide.toString))
            else handleNormalExpr(oneRuleSide.toString)
          }
        }).toList.unzip

        if (!bothSideResult.reduce(_ && _)) false
        else leftValue == rightValue
      }
    } catch {
      case e: Exception => false
    }
  }


  /**
   * 规则处理
   * @param subRule
   * @param datas
   * @return
   */
  def handleRule1(subRule: String, datas: Map[String, Any]): Boolean = {
    val keys = new Regex("d[0-9]+").findAllIn(subRule).toList.sortBy(-_.length) // 根据正则表达式筛选出messages表的rule字段中需要替换成实际值的内容
    var compareExpr = subRule

    try {
      keys.foreach(key => {
        compareExpr = compareExpr.replace(key, datas(key.replace("d", "")).toString)
      })

      val ruleEle = compareExpr.split("=")
      // 如果规则的等号两端没有条件，则核对失败
      if (ruleEle.length != 2 || compareExpr.indexOf("=") == 0 || compareExpr.indexOf("=") == compareExpr.length - 1)
        false
      else {
        val (ruleLeft, ruleRight) = (ruleEle(0), ruleEle(1))
        val leftValue = CalcStrExpression.evaluateStr(ruleLeft)
        val rightValue =  ruleRight match {
          case value if (value.contains("sum:")) => {
            0
          }// 对求和的计算
          case _ => CalcStrExpression.evaluateStr(ruleRight)
        }

        leftValue == rightValue
      }
    } catch { // 规则处理时出现异常，则本条规则核对失败
      case e: Exception => false
    }
  }



  def splitRules(rule: String) = {
    rule match {
      case value if (value.contains("||")) => ("OR", value.split("\\|\\|").toList)
      case value if (value.contains("&&")) => ("AND", value.split("\\&\\&").toList)
      case _ => ("ONE", List(rule))
    }
  }

  /**
   * 根据数据与规则进行核对
   * @param datas
   * @param rule
   * @return
   */
  def ruleAnalysis(datas: List[Elements], rule: String): Boolean = {
    val rules = splitRules(rule)
    val subRules = rules._2.asInstanceOf[List[String]]
    val result = rules._1 match {
      case "AND" => subRules.forall(subRule => handleRule(subRule, datas)) // 与的情况，需要每一条记录都满足
      case _ => subRules.exists(subRule => handleRule(subRule, datas)) // 或以及只有一条规则的情况，只要一条记录满足
    }
    result
  }
  

  def main(args: Array[String]) {
    val dataList = List(Elements("1", "12"), Elements("2", "23"), Elements("3", "12"))

    val rule = "d3=d1&&d3=d2"
    val result = ruleAnalysis(dataList, rule)

    println(s"规则：$rule")
    println(s"数据：$dataList")
    println(s"核对结果为： $result")
  }
  
}