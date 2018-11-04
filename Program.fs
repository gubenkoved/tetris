open System
open System.Drawing
open System.Windows.Forms

// ------------------------------------------------------------------------
type IntPoint =
   struct
      val X: int
      val Y: int

      new(x: int, y: int) = { X = x; Y = y }

      static member Add( p1 : IntPoint, p2 : IntPoint) =
        new IntPoint(p1.X + p2.X, p1.Y + p2.Y)
   end

[<AllowNullLiteral>]
type Block () =
    inherit Object()

    member this.Draw (graphics : Graphics, position : Point, size : Size) = 
        graphics.FillRectangle(new SolidBrush(Color.WhiteSmoke), position.X, position.Y, size.Width, size.Height)

[<AllowNullLiteral>]
type Figure  =
        val Blocks : Block[,]

        new (blocks : Block[,]) = { Blocks = blocks }

        new (mas : int [,]) =
            let n = mas.GetLength(0)
            Figure.deleteEmptyRowAndColumns(mas)
            new Figure(Array2D.init n n (fun i j -> if mas.[i,j] = 1 then new Block() else null))
        
        new (mas : int list list) = 
            new Figure(array2D mas)

        member public this.Size : int = 
            this.Blocks.GetLength(0)

        static member private emptyColumn(m : int [,], col : int) : bool = 
            let mutable result = true
            let n = m.GetLength(0)
            for i = 0 to n - 1 do
                if not(m.[i,col] = 0) then
                    result <- false
            result

        static member private delColumn(m : int [,], col : int) =
            let n = m.GetLength(0)
            for j = col to n - 1 do
                for i = 0 to n - 1 do
                    if j = n - 1 then m.[i,j] <- 0
                        else m.[i, j] <- m.[i, j + 1]

        static member private emptyRow(m : int [,], row : int) : bool = 
            let mutable result = true
            let n = m.GetLength(0)
            for j = 0 to n - 1 do
                if not(m.[row, j] = 0) then
                    result <- false
            result

        static member private delRow(m : int [,], row : int) =
            let n = m.GetLength(0)
            for i = row to n - 1 do
                for j = 0 to n - 1 do
                    if i = n - 1 then m.[i,j] <- 0
                    else m.[i, j] <- m.[i + 1, j]

        static member private deleteEmptyRowAndColumns (m : int [,]) =
            while Figure.emptyColumn(m, 0) do
                Figure.delColumn(m, 0)

            while Figure.emptyRow(m, 0) do
                Figure.delRow(m, 0)

let Figures : int list list list = 
    [[[1; 1; 1; 1]
      [0; 0; 0; 0]
      [0; 0; 0; 0]
      [0; 0; 0; 0]]

     [[0; 0; 0; 0]
      [0; 0; 0; 0]
      [1; 0; 0; 0]
      [1; 1; 1; 0]]

     [[0; 0; 0; 0]
      [0; 0; 0; 0]
      [0; 0; 1; 0]
      [1; 1; 1; 0]]

     [[1; 1; 0; 0]
      [1; 1; 0; 0]
      [0; 0; 0; 0]
      [0; 0; 0; 0]]

     [[0; 0; 0; 0]
      [0; 0; 0; 0]
      [0; 1; 1; 0]
      [1; 1; 0; 0]]

     [[0; 0; 0; 0]
      [0; 0; 0; 0]
      [1; 1; 0; 0]
      [0; 1; 1; 0]]

     [[0; 0; 0; 0]
      [0; 0; 0; 0]
      [0; 1; 0; 0]
      [1; 1; 1; 0]]]

type MoveFigureDownResult =
    | New_Figure_Generated
    | New_Figure_Not_Generated

type FigureGenerationResult =
    | Successful
    | Fail

type FigurePositionCheckingResult =
    | Valid
    | Invalid

type TetrisField (?rows : int, ?columns : int) =
    [<DefaultValue>] val mutable Graphics : Graphics
    [<DefaultValue>] val mutable FieldImage : Image
    [<DefaultValue>] val mutable Drawing_field_size : Size
    [<DefaultValue>] val mutable Score : int

    let mutable _elapsed_ms = 0
    let mutable _last_figure_move_ms = 0
    let _move_figure_every_ms = 300
    
    let mutable _player_lose = false
    let mutable _start_time = Environment.TickCount
    let _accelerate_ratio = 1.1 // ускорение за 1 минуту

    let scoreChangedEvent = new Event<_>()
    [<CLIEvent>]
    member this.ScoreChanged = scoreChangedEvent.Publish

    let playerLoseEvent = new Event<_>()
    [<CLIEvent>]
    member this.PlayerLose = playerLoseEvent.Publish

    let _rows : int = defaultArg rows 26
    let _columns : int = defaultArg columns 17
    let _block_indention : int = 2

    let _field : Block [,] = Array2D.init _rows _columns (fun i j -> null)
    let _random = new Random()
    
    let mutable _figure : Figure = null
    let mutable _figure_position : IntPoint = new IntPoint()

    member private this.blockSize () =
        let block_width = (this.Drawing_field_size.Width - _block_indention * (_columns - 1)) / _columns
        let block_height = (this.Drawing_field_size.Height - _block_indention * (_rows - 1)) / _rows
        
        new Size(block_width, block_height)

    member private this.blockPosition (int_position : IntPoint, block_size : Size) =
        new Point(_block_indention * int_position.X + int_position.X * block_size.Width, _block_indention * int_position.Y + int_position.Y * block_size.Height)

    member private this.drawBlock (block : Block, position : IntPoint, block_size : Size) = 
        let position = this.blockPosition(position, block_size)
        block.Draw(this.Graphics, position, block_size)    

    member private this.drawFigure (block_size : Size) = 
        for i = 0 to _figure.Size - 1 do
            for j = 0 to _figure.Size - 1 do
                let block = _figure.Blocks.[i,j]
                if not(block = null) then                    
                    this.drawBlock(block, IntPoint.Add(new IntPoint(i, j), _figure_position), block_size)

    member private this.figureToField () : unit =
        for row = 0 to _figure.Size - 1 do
            for col = 0 to _figure.Size - 1 do
                let block = _figure.Blocks.[col, row]
                let block_row = row + _figure_position.Y
                let block_col = col + _figure_position.X
                if not(block = null) then
                    _field.[block_row, block_col] <- block
    
    member private this.generateNewFigure () : FigureGenerationResult =
        let i = int(Math.Round(_random.NextDouble() * float(Figures.Length - 1)))
        _figure <- new Figure(Figures.[i])
        _figure_position <- new IntPoint((_columns - _figure.Size) / 2, 0)
        if this.checkFigurePosition(_figure, _figure_position) = FigurePositionCheckingResult.Invalid then
            FigureGenerationResult.Fail
        else FigureGenerationResult.Successful

    member private this.moveFigureDown () : MoveFigureDownResult = 
        let mutable figure_generated = MoveFigureDownResult.New_Figure_Not_Generated
        let newPosition = new IntPoint(_figure_position.X, _figure_position.Y + 1)
        if this.checkFigurePosition(_figure, newPosition) = FigurePositionCheckingResult.Invalid then
            this.figureToField()
            if (this.generateNewFigure() = FigureGenerationResult.Fail) then
                _player_lose <- true
                this.onPlayerLose()
            figure_generated <- MoveFigureDownResult.New_Figure_Generated
        else _figure_position <- newPosition
        this.clearFilledLines()
        figure_generated

    member private this.checkFigurePosition (figure : Figure, position : IntPoint) : FigurePositionCheckingResult = 
        let mutable result = FigurePositionCheckingResult.Valid
        for i = 0 to figure.Size - 1 do
            for j = 0 to figure.Size - 1 do
                let block = figure.Blocks.[i,j]
                let b_col = i + position.X
                let b_row = j + position.Y
                if not(block = null) then 
                    if b_col < 0 || b_col >= _columns then
                        result <- FigurePositionCheckingResult.Invalid
                    elif b_row >= _rows || b_row < 0 then
                        result <- FigurePositionCheckingResult.Invalid
                    elif not(_field.[b_row, b_col] = null) then
                        result <- FigurePositionCheckingResult.Invalid
        result

    member private this.onScoreChanged() =
        scoreChangedEvent.Trigger(this)

    member private this.onPlayerLose() =
        playerLoseEvent.Trigger(this)

    member this.drawFieldDecorations () = 
        let ul_bound = this.blockPosition(new IntPoint(), this.blockSize())
        let dr_bound = Point.Add(this.blockPosition(new IntPoint(_columns - 1, _rows - 1), this.blockSize()), this.blockSize())
        this.Graphics.DrawRectangle(new Pen(Color.FromArgb(100, Color.White), 2.0f), ul_bound.X + 1, ul_bound.Y + 1, dr_bound.X - 2, dr_bound.Y - 3)

    member this.checkFillingLine (row : int) : bool =
        let mutable result = true
        for j = 0 to _columns - 1 do
            if _field.[row, j] = null then
                result <- false
        result

    member this.clearFilledLine (row : int) =
        for r = row downto 1 do
            for c = 0 to _columns - 1 do
                _field.[r, c] <- _field.[r - 1, c]
        for c = 0 to _columns - 1 do
            _field.[0, c] <- null

    member this.clearFilledLines () =
        let mutable lines_filled = 0
        for i = _rows - 1 downto 0 do
            while this.checkFillingLine (i) = true do
                this.clearFilledLine (i)
                lines_filled <- lines_filled + 1
        this.Score <- this.Score + 100 * lines_filled * lines_filled
        this.onScoreChanged()

    let colorMatrixElements : float32 [] [] = 
        [|
            [| 1.0f;  0.0f;  0.0f;  0.0f; 0.0f |]       // red scaling factor of 1
            [| 0.0f;  1.0f;  0.0f;  0.0f; 0.0f |]       // green scaling factor of 1
            [| 0.0f;  0.0f;  1.0f;  0.0f; 0.0f |]       // blue scaling factor of 1
            [| 0.0f;  0.0f;  0.0f;  1.0f; 0.0f |]       // alpha scaling factor of 1
            [| 0.0f;  0.0f;  0.0f;  -0.3f; 1.0f |]       // three translations
        |]    

    let mutable imageAttributes =       // color matrix setting happening on game init
        new Imaging.ImageAttributes()

    member this.Draw () =
        //this.Graphics.Clear(Color.Transparent)
        let copy = this.FieldImage.Clone() :?> Image
        
        this.Graphics.Clear(Color.Transparent)
        this.Graphics.DrawImage(
           copy,
           new Rectangle(0, 0, this.FieldImage.Width, this.FieldImage.Height),  // destination rectangle 
           0, 0,        // upper-left corner of source rectangle 
           this.FieldImage.Width,       // width of source rectangle
           this.FieldImage.Height,      // height of source rectangle
           GraphicsUnit.Pixel,
           imageAttributes)
        this.drawFigure(this.blockSize())
        for row = 0 to _rows - 1 do
            for col = 0 to _columns - 1 do
                let block = _field.[row, col]
                if not(block = null) then
                    this.drawBlock(block, new IntPoint(col, row), this.blockSize())
        this.drawFieldDecorations()

    member this.Tick (interval : int) = 
        if not _player_lose then
            this.Draw()
            _elapsed_ms <- _elapsed_ms + interval
            let time_of_playing_min = float(Environment.TickCount - _start_time) / (60.0 * 1000.0)
            let next_figure_through = float(_move_figure_every_ms) / Math.Pow(_accelerate_ratio, time_of_playing_min)
            if (float(_elapsed_ms - _last_figure_move_ms) > next_figure_through ) then 
                this.moveFigureDown () |> ignore
                _last_figure_move_ms <- _elapsed_ms

    member this.MoveFigureLeft () =
        let newPosition = new IntPoint(_figure_position.X - 1, _figure_position.Y)
        if this.checkFigurePosition(_figure, newPosition) = FigurePositionCheckingResult.Valid then
            _figure_position <- newPosition

    member this.MoveFigureRight () =
        let newPosition = new IntPoint(_figure_position.X + 1, _figure_position.Y)
        if this.checkFigurePosition(_figure, newPosition) = FigurePositionCheckingResult.Valid then
            _figure_position <- newPosition

    member this.RotateFigureCounterClockwise () =
        let new_figure = new Figure(Array2D.init _figure.Size _figure.Size (fun i j ->
            let new_i = _figure.Size - j - 1
            let new_j = i
            if (_figure.Blocks.[new_i, new_j] = null) then 0
            else 1))
        if this.checkFigurePosition(new_figure, _figure_position) = FigurePositionCheckingResult.Valid then
            _figure <- new_figure

    member this.RotateFigureClockwise () =
        let new_figure = new Figure(Array2D.init _figure.Size _figure.Size (fun i j ->
            let new_i = j
            let new_j = _figure.Size - i - 1
            if (_figure.Blocks.[new_i, new_j] = null) then 0
            else 1))
        if this.checkFigurePosition(new_figure, _figure_position) = FigurePositionCheckingResult.Valid then
            _figure <- new_figure

    member this.DropFigureDown () = 
        let mutable figure_generated = MoveFigureDownResult.New_Figure_Not_Generated
        while figure_generated = MoveFigureDownResult.New_Figure_Not_Generated do
            figure_generated <- this.moveFigureDown()

    member this.InitGame () : unit =
        this.generateNewFigure() |> ignore
        let colorMatrix = new Imaging.ColorMatrix(colorMatrixElements)
        imageAttributes.SetColorMatrix(colorMatrix)
// ------------------------------------------------------------------------

let refresh_per_second = 30.0
let refresh_interval = int(1000.0 / refresh_per_second)

let backgound_finded = IO.File.Exists("background.jpg")

let main_window = new Form (Text = "Tertis F#. Gubenkov D. Eugene") 
main_window.StartPosition <- FormStartPosition.CenterScreen
main_window.Height <- 700
main_window.Width <- 580
main_window.WindowState <- FormWindowState.Normal
main_window.MaximizeBox <- false
main_window.Padding <- new Padding(20)
main_window.FormBorderStyle <- FormBorderStyle.FixedSingle
main_window.BackColor <- Color.Gray
if backgound_finded then main_window.BackgroundImage <- Bitmap.FromFile("background.jpg")
main_window.BackgroundImageLayout <- ImageLayout.Stretch

let panel = new Panel()
main_window.Controls.Add(panel)
panel.Dock <- DockStyle.Fill
panel.BackColor <- Color.Transparent

let picture_box = new PictureBox()
panel.Controls.Add(picture_box)
picture_box.Dock <- DockStyle.Right
picture_box.Size <- new Size(400, 0)

let score_label = new Label()
panel.Controls.Add(score_label)
score_label.Font <- new Font("Cambria", 16.0f)
score_label.AutoSize <- true
score_label.ForeColor <- Color.White

let player_lose_label = new Label()
panel.Controls.Add(player_lose_label)
player_lose_label.Font <- new Font("Cambria", 24.0f)
player_lose_label.AutoSize <- true
player_lose_label.ForeColor <- Color.White
player_lose_label.Dock <- DockStyle.Fill
player_lose_label.TextAlign <- ContentAlignment.MiddleCenter
player_lose_label.Text <- "Game over"
player_lose_label.Visible <- false
player_lose_label.AutoSize <- false

let max_x = picture_box.Width
let max_y = picture_box.Height

let bmp = new System.Drawing.Bitmap(max_x, max_y)
picture_box.Image <- bmp

let graphics = System.Drawing.Graphics.FromImage(bmp)
graphics.SmoothingMode <- Drawing2D.SmoothingMode.AntiAlias

let update_scores = (fun (field : TetrisField) -> 
    score_label.Text <- "Score: " + field.Score.ToString())

let show_player_lose = (fun (field : TetrisField) ->
    picture_box.Visible <- false
    picture_box.Dock <- DockStyle.None
    player_lose_label.Visible <- true)

// -------------------------------------------------
let field = new TetrisField()
field.Drawing_field_size <- new Size(max_x, max_y)
field.FieldImage <- bmp
field.Graphics <- graphics
field.ScoreChanged.Add(update_scores)
field.PlayerLose.Add(show_player_lose)
field.InitGame()
update_scores(field)
// -------------------------------------------------

let game_tick = (fun arg ->     
    field.Tick(refresh_interval)
    picture_box.Image <- bmp)

let timer = new Timer()
timer.Interval <- refresh_interval
timer.Tick.Add(game_tick)
timer.Start()

main_window.Shown.Add((fun (a) -> field.Draw()))
main_window.KeyDown.Add((fun args ->
        if args.KeyCode = Keys.Left then
            field.MoveFigureLeft()
        elif args.KeyCode = Keys.Right then
            field.MoveFigureRight()
        elif args.KeyCode = Keys.Up then
            field.RotateFigureCounterClockwise()
        elif args.KeyCode = Keys.Down then
            field.RotateFigureClockwise()
        elif args.KeyCode = Keys.Space then
            field.DropFigureDown()))

Application.Run(main_window)