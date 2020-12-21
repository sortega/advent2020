package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._
import scala.util.matching.Regex

import aoc.CardinalPoint._
import aoc.Pos.Syntax._
import com.google.ortools.Loader
import com.google.ortools.constraintsolver._

object Day21 {
  Loader.loadNativeLibraries()

  case class Recipe(ingredients: List[String], alergens: List[String])

  object Recipe {
    def parse(input: String): Recipe = input match {
      case s"$ingredients (contains $alergens)" =>
        Recipe(ingredients.split(" ").toList, alergens.split(", ").toList)
    }
  }

  def findAlergens(recipes: List[Recipe]): List[Map[String, Option[String]]] = {
    val model       = new Solver("AlergenSolver")
    val ingredients = recipes.flatMap(_.ingredients).distinct.toArray
    val alergens    = ("" :: recipes.flatMap(_.alergens).distinct).toArray

    val ingredientAlergens =
      model.makeIntVarArray(ingredients.size, 0, alergens.size - 1, "alergens")
    model.addConstraint(model.makeAllDifferentExcept(ingredientAlergens, 0))

    recipes.foreach { recipe =>
      val recipeAlergens = recipe.ingredients.map { name =>
        ingredientAlergens(ingredients.indexOf(name))
      }.toArray
      recipe.alergens.foreach { alergenName =>
        val alergen = alergens.indexOf(alergenName)
        model.addConstraint(model.makeCount(recipeAlergens, alergen, 1))
      }
    }

    val decisionBuilder =
      model.makePhase(ingredientAlergens, Solver.CHOOSE_FIRST_UNBOUND, Solver.ASSIGN_MIN_VALUE)
    model.newSearch(decisionBuilder)

    LazyList
      .unfold(model) { model =>
        if (model.nextSolution()) Some {
          val solution = (for {
            i <- ingredients.indices
            name         = ingredients(i)
            alergenIndex = ingredientAlergens(i).value().toInt
            alergen      = Some(alergens(alergenIndex)).filter(_.nonEmpty)
          } yield name -> alergen).toMap
          (solution, model)
        }
        else None
      }
      .toList
  }

  def part1(input: List[Recipe], alergens: Map[String, Option[String]]): Int = {
    val safeIngredients = alergens.collect { case (ingredient, None) =>
      ingredient
    }.toSet
    input.flatMap(_.ingredients).count(safeIngredients.contains)
  }

  def part2(alergens: Map[String, Option[String]]): String = alergens
    .collect { case (ingredient, Some(alergen)) =>
      (ingredient, alergen)
    }
    .toList
    .sortBy(_._2)
    .map(_._1)
    .mkString(",")

  def main(args: Array[String]): Unit = {
    val input     = parseInputLines(day = 21)(Recipe.parse)
    val solutions = findAlergens(input)
    require(solutions.size == 1, "Non unique solution")
    println(part1(input, solutions.head))
    println(part2(solutions.head))
  }
}
