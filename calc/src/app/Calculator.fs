module main
open calc
open System.Windows.Forms
open System.Drawing

let mutable st = " "

let programLabel =
    let lbl = new Label()
    lbl.Location <- Point(20, 10)
    lbl.Size <- Size (320, 30)
    lbl.Font <- new Font("Arial", 16.0f)
    lbl.ForeColor <- Color.Black
    lbl.Text <- sprintf " 0.0" 
    lbl.BackColor <- Color.LightGray
    lbl

let zeroButton (form : Form) =
    let but = new Button()
    but.Font <- new Font("Verdana", 15.0f)
    but.ForeColor <- Color.Black
    but.Text <- "0"
    but.BackColor <- Color.White
    but.Location <- Point (80,240)
    but.Size <- Size (40, 40)
    but.Click.Add(fun e -> st <- st + "0"; programLabel.Text <- sprintf "%s" st)
    but
   
let ravnoButton (form : Form) =
    let but = new Button()
    but.Text <- "="
    but.Font <- new Font("Arial", 15.0f)
    but.ForeColor <- Color.DarkRed
    but.BackColor <- Color.Orange
    but.Location <- Point (290, 204)
    but.Size <- Size (50, 28)
    but.Click.Add(fun e ->  
        try st <- " " + (Obhod (makeTree (MapIt (parser (st), [])))).ToString()
            programLabel.Text <- sprintf "%s" st with
        | :? WrongExpr -> st <- " "
                          programLabel.Text <- sprintf " Wrong Expression!"
        | :? System.OverflowException -> st <- " "
                                         programLabel.Text <- sprintf " Data Overflow!"
        | :? DivByZero -> st <- " "
                          programLabel.Text <- sprintf " Division By Zero!" )
    but

let cButton (form : Form) =
    let but = new Button()        
    but.Font <- new Font("Arial", 14.0f)
    but.ForeColor <- Color.DarkRed
    but.BackColor <- Color.Orange
    but.Text <- "C"
    but.Location <- Point (290, 252)
    but.Size <- Size (50, 28)
    but.Click.Add(fun e -> st <- " "; programLabel.Text <- sprintf " 0.0") 
    but

let m = [|"+"; "-"; "*"; "/"; "%"; "("; ")"; "^"|]

let mainForm =
    let form = new Form(Visible = false)
    form.BackColor <- Color.DimGray
    form.Size <- Size(370, 340)
    form.Controls.Add (programLabel)
    form.Controls.Add (zeroButton form)
    form.Controls.Add (ravnoButton form)
    form.Controls.Add (cButton form)

    for i = 1 to 9 do
        let but = new Button()
        but.ForeColor <- Color.Black
        but.Font <- new Font("Arial", 15.0f)
        but.Location <- Point (140 - ((2 * i) % 3) * 60, 60 + (i - 1)/3 * 60)
        but.BackColor <- Color.White
        but.Size <- Size (40, 40)
        but.Text <- i.ToString()
        but.Click.Add(fun e -> st <- st + i.ToString())
        but.Click.Add(fun e -> programLabel.Text <- sprintf "%s" st)
        form.Controls.Add (but)

    for i = 0 to 7 do
        let but = new Button()
        but.Font <- new Font("Arial", 13.0f)
        but.ForeColor <- Color.Black
        but.BackColor <- Color.DarkGray
        but.Location <- Point (220 + (i / 5) * 70, 60 + (i % 5) * 48)
        but.Size <- Size (50, 28)
        but.Text <- m.[i]
        but.Click.Add(fun e -> st <- st + m.[i]; programLabel.Text <- sprintf "%s" st)
        form.Controls.Add (but)
    form

[<EntryPoint>]
let main argv = 
    mainForm.Visible <- true
    Application.Run () 
    0



