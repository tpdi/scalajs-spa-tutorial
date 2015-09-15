package spatutorial.client.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import spatutorial.client.components.Bootstrap.{CommonStyle, Button}
import spatutorial.shared._
import scalacss.ScalaCssReact._

object TodoList {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class TodoListProps(items: Seq[TodoItem], stateChange: (TodoItem) => Unit, editItem: (TodoItem) => Unit, deleteItem: (TodoItem) => Unit)
  
  
  // I'm "cheating" just a bit: I have no state to mod, but stateChange will send the modification to the server and update all items
  def updatePriority(item: TodoItem, stateChange: (TodoItem) => Unit)(e: ReactEventI) = {
      // update TodoItem priority
      val newPri = e.currentTarget.value match {
        case p if p == TodoHigh.toString => TodoHigh
        case p if p == TodoNormal.toString => TodoNormal
        case p if p == TodoLow.toString => TodoLow
      }
      stateChange(item.copy(priority = newPri))
    }

  val TodoList = ReactComponentB[TodoListProps]("TodoList")
    .render(P => {
    val style = bss.listGroup
    def renderItem(item: TodoItem) = {
      // convert priority into Bootstrap style
      val itemStyle = item.priority match {
        case TodoLow => style.itemOpt(CommonStyle.info)
        case TodoNormal => style.item
        case TodoHigh => style.itemOpt(CommonStyle.danger)
      }
      <.li(itemStyle)(
        <.input(^.tpe := "checkbox", ^.checked := item.completed, ^.onChange --> P.stateChange(item.copy(completed = !item.completed))),
        <.span(" "),
        if (item.completed) <.s(item.content) else <.span(item.content),
        Button(Button.Props(() => P.editItem(item), addStyles = Seq(bss.pullRight, bss.buttonXS)), "Edit"),
        Button(Button.Props(() => P.deleteItem(item), addStyles = Seq(bss.pullRight, bss.buttonXS)), "Delete"),
        <.select(bss.pullRight, ^.id := "priority", ^.value := item.priority.toString, ^.onChange ==> updatePriority(item, P.stateChange),
          <.option(^.value := TodoHigh.toString, "High"),
          <.option(^.value := TodoNormal.toString, "Normal"),
          <.option(^.value := TodoLow.toString, "Low")
        )
      )
    }
    <.ul(style.listGroup)(P.items map renderItem)
  })
    .build

  def apply(props: TodoListProps) = TodoList(props)
}
