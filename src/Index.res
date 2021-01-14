open Belt
@bs.val external document: {..} = "document"
@bs.val external window: {..} = "window"
@bs.module external escapeHtml: string => string = "./escapeHtml"

module Editor = {
  type cursor = {
    x: int,
    y: int,
  }

  type t = {
    cursor: cursor,
    text: array<string>,
  }

  let escape = s => {
    let s = escapeHtml(s)
    Js.String2.replaceByRe(s, %re("/ /g"), "&nbsp;")
  }

  let currentLine = t => {
    t.text[t.cursor.y]->Option.getWithDefault("")
  }

  let getLine = (t, y) => {
    t.text[y]
  }

  let getLineAt = (t, y) => {
    t.text[y]->Option.getWithDefault("")
  }

  let cursorSegmented = t => {
    let l = currentLine(t)
    let before = l |> Js.String.substring(~from=0, ~to_=t.cursor.x)
    let after = l |> Js.String.substr(~from=t.cursor.x)
    (before, after)
  }

  let toHtmlString = t => {
    t.text->Array.mapWithIndex((i, l) => {
      if i == t.cursor.y {
        let (beforeCursor, afterCursor) = cursorSegmented(t)
        "<p class='line'><span>" ++
        escape(beforeCursor) ++
        "</span><span id='cursor'></span><span>" ++
        escape(afterCursor) ++ "</span></p>"
      } else {
        let l = Js.String.length(l) > 0 ? l : " "
        "<p class='line'><span>" ++ escape(l) ++ "</span></p>"
      }
    }) |> Js.Array.joinWith("\n")
  }

  let render = (t, dom) => {
    dom["innerHTML"] = toHtmlString(t)
  }

  module TextOperations = {
    let insertLetter = (t, letter) => {
      let (before, after) = cursorSegmented(t)
      {t.text[t.cursor.y] = before ++ letter ++ after} |> ignore
      {...t, cursor: {...t.cursor, x: t.cursor.x + 1}}
    }

    let backspace = t => {
      let x = t.cursor.x
      let y = t.cursor.y
      if x == 0 {
        if y == 0 {
          t
        } else {
          /* First letter in the line, so move the current line to end of prev
           line */
          let curLine = currentLine(t)
          let prevLine = getLineAt(t, y - 1)
          let x = Js.String.length(prevLine)
          let l = prevLine ++ curLine
          t.text |> Js.Array.spliceInPlace(~pos=t.cursor.y, ~remove=1, ~add=[]) |> ignore
          ignore(t.text[y - 1] = l)
          {...t, cursor: {x: x, y: y - 1}}
        }
      } else {
        let l = currentLine(t)
        let l =
          Js.String.substring(~from=0, ~to_=t.cursor.x - 1, l) ++
          Js.String.substringToEnd(~from=t.cursor.x, l)
        ignore(t.text[t.cursor.y] = l)
        {...t, cursor: {...t.cursor, x: t.cursor.x - 1}}
      }
    }

    let carriageReturn = t => {
      let (before, after) = cursorSegmented(t)
      {t.text[t.cursor.y] = before} |> ignore
      t.text |> Js.Array.spliceInPlace(~pos=t.cursor.y + 1, ~remove=0, ~add=[after]) |> ignore
      {...t, cursor: {x: 0, y: t.cursor.y + 1}}
    }

    let arrowLeft = t => {
      let x = t.cursor.x
      let y = t.cursor.y
      let cursor = {
        if x == 0 {
          if y == 0 {
            {x: 0, y: 0}
          } else {
            {x: Js.String.length(getLineAt(t, y - 1)), y: y - 1}
          }
        } else {
          {x: x - 1, y: y}
        }
      }
      {...t, cursor: cursor}
    }

    let arrowRight = t => {
      let x = t.cursor.x
      let y = t.cursor.y
      let l = currentLine(t)
      let lineLen = Js.String.length(l)
      let cursor = {
        if x == lineLen {
          if y + 1 == Js.Array.length(t.text) {
            t.cursor
          } else {
            {x: 0, y: y + 1}
          }
        } else {
          {x: x + 1, y: y}
        }
      }
      {...t, cursor: cursor}
    }

    let arrowDown = t => {
      let x = t.cursor.x
      let y = t.cursor.y
      let cursor = {
        switch getLine(t, y + 1) {
        | Some(nextLine) =>
          let len = Js.String.length(nextLine)
          if x > len {
            {x: len, y: y + 1}
          } else {
            {x: x, y: y + 1}
          }
        | None => t.cursor
        }
      }
      {...t, cursor: cursor}
    }

    let arrowUp = t => {
      let x = t.cursor.x
      let y = t.cursor.y
      let cursor = {
        switch getLine(t, y - 1) {
        | Some(prevLine) => {
            let len = Js.String.length(prevLine)
            if x > len {
              {x: len, y: y - 1}
            } else {
              {x: x, y: y - 1}
            }
          }
        | None => t.cursor
        }
      }
      {...t, cursor: cursor}
    }
  }

  let handleEvent = (tRef, dom, event) => {
    let t = tRef.contents
    let letter = event["key"]
    let isContentKey = {
      /* https://stackoverflow.com/a/38802011 */
      Js.String.length(letter) == 1
    }
    Js.log(letter)
    event["preventDefault"]()
    if isContentKey {
      tRef := TextOperations.insertLetter(t, letter)
    } else if event["shiftKey"] {
      ()
    } else {
      switch letter {
      | "Enter" => tRef := TextOperations.carriageReturn(t)
      | "ArrowLeft" => tRef := TextOperations.arrowLeft(t)
      | "ArrowRight" => tRef := TextOperations.arrowRight(t)
      | "ArrowDown" => tRef := TextOperations.arrowDown(t)
      | "ArrowUp" => tRef := TextOperations.arrowUp(t)
      | "Backspace" => tRef := TextOperations.backspace(t)
      | _ => ()
      }
    }
    render(tRef.contents, dom)
  }
}

/*
- Get event
  - if printable, add it to current cursor position.
  - if arrows, move
  - if large movement, move
  - if delete/bkspc, delete
  - if enter, insert line
 */

let init = editorDom => {
  let state: Editor.t = {
    cursor: {x: 3, y: 0},
    text: ["Hello,", "This is a long line."],
  }
  let state: ref<Editor.t> = ref(state)

  editorDom["addEventListener"]("keydown", event => {
    Editor.handleEvent(state, editorDom, event)
  }) |> ignore

  editorDom["focus"]() |> ignore
  Editor.render(state.contents, editorDom)
}

let start = () => {
  let editorDom = document["getElementById"]("editor")
  Js.log("Hello!")
  init(editorDom)
}

window["onload"] = start
