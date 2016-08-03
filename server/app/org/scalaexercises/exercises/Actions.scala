/*
 * scala-exercises-server
 * Copyright (C) 2015-2016 47 Degrees, LLC. <http://www.47deg.com>
 */

package org.scalaexercises.exercises

import scala.concurrent.Future
import play.api.Play
import play.api.Play.current
import play.api.mvc._
import play.api.Logger

case class Secure[A](action: Action[A]) extends Action[A] {
  def apply(request: Request[A]): Future[Result] = {
    action(request)
  }

  lazy val parser = action.parser
}
