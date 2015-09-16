package spatutorial.client.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import spatutorial.client.components.Bootstrap.{CommonStyle, Button}
import spatutorial.shared._
import scalacss.ScalaCssReact._
import spatutorial.client.logger._
import chandu0101.scalajs.react.components.textfields.ReactTagsInput

object TodoList {
  // shorthand for styles
  @inline private def bss = GlobalStyles.bootstrapStyles

  case class TodoListProps(items: Seq[TodoItem], stateChange: (TodoItem) => Unit, editItem: (TodoItem) => Unit, deleteItem: (TodoItem) => Unit)
  
  
  // I'm "cheating" just a bit: I have no component state to modify, 
  // but stateChange will send the modification to the server and then update all items and their states
  def updatePriority(item: TodoItem, stateChange: (TodoItem) => Unit)(e: ReactEventI) = {
      // update TodoItem priority; note that foreach is called on an Option, so it's 0 or 1 at most
      TodoPriority(e.currentTarget.value).foreach( newPri => stateChange(item.copy(priority = newPri)))
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
      
      // note that P is captured by the dropDown closure/local function, so we don't need to pass it, or its stateChange, to the function
      // let's take this further: by moving dropDown into renderItem, we capture item as well,
      // so we can remove the item argument to dropdown
      // But wait! Now we don't even need a function!
      val dropDown = if (!item.completed) 
          <.select(bss.pullRight, ^.value := item.priority.toString, ^.onChange ==> updatePriority(item, P.stateChange),
                <.option(^.value := TodoHigh.toString, "High"),
                <.option(^.value := TodoNormal.toString, "Normal"),
                <.option(^.value := TodoLow.toString, "Low")
          )
        else <.span(); // the semi-colon is optional, but it aids readability
      
      <.li(itemStyle)(
        <.input(Seq(bss.pullLeft, GlobalStyles.padRight), ^.tpe := "checkbox", ^.checked := item.completed, 
            ^.onChange --> P.stateChange(item.copy(completed = !item.completed))),
        //<.span(" "),
        //ReactTagsInput(ref = "uncontrolledtags"),
        <.span(^.onClick ==> ((e: ReactEventH) => { P.editItem(item);}))(if (item.completed) <.s(item.content) else <.span(item.content)),
        <.div(bss.pullRight)(
            Button(Button.Props(() => P.editItem(item), addStyles = Seq(bss.pullRight, bss.buttonXS)), "Edit"),
            Button(Button.Props(() => P.deleteItem(item), addStyles = Seq(bss.pullRight, bss.buttonXS)), "Delete"),
            
            // show the priority dropdown only if the item is not completed
            // inline (scala's if is an expression, not a statement, like the ternary in other languages)...
            if (!item.completed) 
              <.select(bss.pullRight, ^.value := item.priority.toString, ^.onChange ==> updatePriority(item, P.stateChange),
                <.option(^.value := TodoHigh.toString, "High"),
                <.option(^.value := TodoNormal.toString, "Normal"),
                <.option(^.value := TodoLow.toString, "Low") 
              )
            else 
              <.span()
          // or as a helper function if inline becomes too complex
          , dropDown()
        )
      )
    }
    
    <.ul(style.listGroup)(P.items map renderItem)
  })
    .build

  def apply(props: TodoListProps) = TodoList(props)
}
