#Mini-SDL2
SDL2の代表的な機能をCommon Lispから簡単に利用するための補助ライブラリ。  
(SDL2のAPIラッパーではありません)

##Install

###Required
[Common Lisp] CFFI

[OS] SDL2, SDL2\_image, SDL2\_ttf, SDL2\_mixer

##Usage
###General
#### with-sdl2
    sdl2:with-sdl2 flags &body body
SDL2を使用したプログラムを実行する。  
flagsには、:video :audio :joystickが有効である。

#### set-error
    sdl2:set-error &rest fmt
SDLエラーを設定する。  
fmtにはcl:formatに順ずる。

#### get-error
    sdl2:get-error
SDLエラーを取得する。

#### clear-error
    sdl2:clear-error
SDLエラーをクリアする。

###Event-Handling
#### loop-event-handling
    sdl2loop-event-handling &optional handle
                            &key window
                                 keyboard
                                 text-editing
                                 text-input
                                 mouse-motion
                                 mouse-button
                                 mouse-wheel
                                 joy-axis
                                 joy-ball
                                 joy-button
                                 joy-device
                                 quit
                                 idle
イベントハンドリングループを開始する。  
handleはループ中にイベント情報を受け取るハンドラが束縛される変数名である。  
各キーワードには、キーワードが示すイベント発生時に実行する式を記述する。   
idleはイベントが存在しないときに実行される式である。

#### quit-request
    sdl2:quit-request
quitイベントをイベントキューに追加する。

#### leave-event-loop
    sdl2:leave-event-loop
loop-event-handlingのループ処理から脱出するフラグを設定する。  
次のイベントポーリング前にループ処理は終了する。

#### with-event-slot
    sdl2:with-event-slot event-type (handle &rest keys) &body body
イベントハンドラからイベント情報を取得する。  
keysにはevent-typeによって異なるキーワード引数が設定される。
* :window
    * :event - :window-event
    * :timestamp - イベントのタイムスタンプ値
    * :window-id - windowのID
    * :win-event - WindowEventを識別するシンボル
    * :data1 - WindowEventのパラメータ
    * :data2 - WindowEventのパラメータ
* :keyboard
    * :event - :keyup または :keydown
    * :timestamp - イベントのタイムスタンプ値
    * :window-id - windowのID
    * :state - :pressed または :released
    * :repeat - リピート判別値
    * :keysym - キーの識別シンボル
* :text-editing
    * :event - :text-editing
    * :timestamp - イベントのタイムスタンプ値
    * :window-id - windowのID
    * :text - 編集中のテキスト
    * :start - テキスト中の編集開始位置
    * :length - 編集したテキストの長さ
* :text-input
    * :event - :text-input
    * :timestamp - イベントのタイムスタンプ値
    * :window-id - windowのID
    * :text - 入力されたテキスト
* :mouse-motion
    * :event - :mouse-motion
    * :timestamp - イベントのタイムスタンプ値
    * :window-id - windowのID
    * :state - ボタンの状態
    * :x - マウスのx座標
    * :y - マウスのy座標
    * :xrel - マウスの相対x座標
    * :yrel - マウスの相対y座標
* :mouse-button
    * :event - :mouse-button-down または :mouse-button-up
    * :timestamp - イベントのタイムスタンプ値
    * :window-id - windowのID
    * :button - ボタン番号
    * :state - :pressed または :released
    * :x - マウスのx位置
    * :y - マウスのy座標
* :mouse-wheel
    * :event - :mouse-wheel
    * :timestamp - イベントのタイムスタンプ値
    * :window-id - windowのID
    * :x - マウスのx座標
    * :y - マウスのy座標
* :joy-axis
    * :event - :joy-axis-motion
    * :timestamp - イベントのタイムスタンプ値
    * :which - ジョイスティック番号
    * :axis - 軸番号
    * :value - 値
* :joy-ball
    * :event - :joy-ball-motion
    * :timestamp - イベントのタイムスタンプ値
    * :which - ジョイスティック番号
    * :ball - ボール番号
    * :xrel - ボールの相対x座標
    * :yrel - ボールの相対y座標
* :joy-hat
    * :event - :joy-hat-motion
    * :timestamp - イベントのタイムスタンプ値
    * :which - ジョイスティック番号
    * :hat - ハット番号
    * :value - 値
* :joy-button
    * :event - :joy-button-down または :joy-buttn-up
    * :timestamp - イベントのタイムスタンプ値
    * :which - ジョイスティック番号
    * :button - ボタン番号
    * :state - :pressed または :released
* :quit
    * :event - :quit
    * :timestamp - イベントのタイムスタンプ値

###Video (Required init with :video)
#### with-window
    sdl2:with-window (name &key title x y w h flags) &body body
SDL2ウィンドウを生成する。

####with-context
    sdl2:with-context name win &body body
SDL2ウィンドウに対応したGL描画コンテキストを生成する。

#### with-window-and-context
    sdl2:with-window-and-context (win ctx &key title x y w h flags) &body body
SDL2ウィンドウとGL描画コンテキストを生成する。

#### begin-frame
    sdl2:begin-frame win ctx &body body
描画フレームを開始する。  
フレームの開始時にctxがカレントとして設定され、  
フレームの終了時にwinのバックバッファがスワップされる。

#### load-image
    sdl2:load-image pathname
pathnameのイメージフォーマットをロードする。  
imageオブジェクトまたは読み込みに失敗した場合はnilが返る。

#### close-image
    sdl2:close-image image
imageオブジェクトを解放する。  
bind-gltexでバインディングしたgl-textureは自動的に解放されない。

#### bind-gltex
    sdl2:bind-gltex image
imageをカレントのGLコンテキストにテクスチャとしてバインドする。  
バインド成功した場合、返り値はGLのテクスチャ番号、失敗した場合はnilが返る。  
初回のバインド時にはgen-textureが発生する。

#### load-font
    sdl2:load-font pathname ptsize index
pathnameのフォント(.ttf)をロードする。  
fontオブジェクトまたは読み込みに失敗した場合はnilが返る。

#### close-font
    sdl2:close-font font
fontオブジェクトを解放する。

#### font-attribute
    sdl2:font-attribute attr font (&rest value)
fontのスタイルを取得・設定する。  
attrには:style :outline :hinting :kerningが指定できる。  
valueが指定された場合は、新しい属性としてvalueが設定される。
* :style - 以下のキーワードの組み合わせリスト
    * :bold
    * :italic
    * :underline
    * :strikethroug
* :outline - ピクセルサイズ
* :hinting - 以下のキーワードのいずれか一つ
    * :normal
    * :light
    * :mono
    * :none
* :kerning - tまたはnil

#### render-text
    sdl2:render-text render-mode font text &optional fr fg fb fa br bg bb ba
fontからレンダリング結果のimageオブジェクトを生成する。  
render-modeは:solid :shaded :blendedが指定できる。  
fr-faはフォアカラー、br-baはバックカラーの各要素(0-255)となる。

###Input (Required init with :joystick)
#### num-joysticks
    sdl2:num-joysticks
認識されているジョイスティックデバイスの個数を取得する。

#### joystick-style
    sdl2:joystick-style index
ジョイスティックの構造を取得する。  
返り値はリストで、(name num-axes num-balls num-buttons num-hats)を表す。

#### joystick-open
    sdl2:joystick-open index
ジョイスティックを使用開始する。

#### joystick-close
    sdl2:joystick-close index
ジョイスティックの使用を終了する。

#### joystick-opened
    sdl2:joystick-opened index
指定した番号のジョイスティックを使用しているか判定する。

#### list-active-joysticks
    sdl2:list-active-joysticks
使用中のジョイスティックIDのリストを取得する。

###Audio (Required init with :audio)
#### with-audio
    sdl2:with-audio (&key freqency format channels chunksize) &body body
オーディオデバイスの使用を開始する。

#### load-wave
    sdl2:load-wave pathname
pathnameの音声ファイルをロードする。  
sampleオブジェクトまたは、音声ファイルの読み込みに失敗した場合はnilが返る。

#### close-sample
    sdl2:close-sample sample
音声ファイルを解放する。

#### load-music
    sdl2:load-music pathname
pathnameから音楽ファイルをロードする。  
musicオブジェクトまたは読み込みに失敗した場合はnilが返る。

#### close-music
    sdl2:close-music music
musicオブジェクトを解放する。

#### channels
    sdl2:channels &optional count
ミキシングのチャンネル数を設定する。  
countを省略した場合、最大で設定可能なチャンネル数を取得する。

#### volume
    sdl2:volume &optional channel-or-music volume
チャンネルのボリュームを設定する。  
volumeを省略した場合、ボリューム設定はせずに設定値のみ取得する。  
channel-or-musicがnilの場合、全てのチャンネルを対象に操作する。  
channel-or-musicが:musicの場合は、音楽再生のボリュームを設定する。

#### play
    sdl2:play channel-or-music sample-or-music &optional loops ms
チャンネルにサンプルを割り当てて再生する。  
msが指定された場合はフェードイン開始する。  
channel-or-musicがnilの場合、空きチャンネルを自動的に選択する。  
channel-or-musicが:musicの場合、音楽再生を開始する。

#### pause
    sdl2:pause &optional channel-or-music
チャンネルの再生を一時停止する。  
channel-or-musicがnilの場合、全てのチャンネルを一時停止する。  
channel-or-musicが:musicの場合、音楽再生を一時停止する。

#### resume
    sdl2:resume &optional channel-or-music
チャンネルの再生を再開する。  
channel-or-musicがnilの場合、全てのチャンネルを再開する。  
channel-or-musicが:musicの場合、音楽再生を一時停止する。

#### halt
    sdl2:halt &optional channel-or-music ms
チャンネルの再生を停止する。  
msが指定された場合はフェードアウト終了する。  
channel-or-musicがnilの場合、全てのチャンネルを停止する。  
channel-or-musicが:musicの場合、音楽再生を停止する。

#### playing
    sdl2:playing &optional channel-or-music
チャンネルが再生中か判別する。  
channel-or-musicがnilの場合、再生中チャンネル数を計算する。  
channel-or-musicが:musicの場合、音楽再生中か判別する。

##Sample Code
    ; On Window Event
    (defun on-window-event (event window)
      (with-event-slot :window (event :event-type evtype
                                      :timestamp time
                                      :window-id id
                                      :event winev
                                      :data1 d1
                                      :data2 d2)
        (format t "~S~%" (list evtype time id winev d1 d2))))

    ; Start SDL2 Program
    (with-sdl2 (:video)
      (with-window (win :x 200 :y 200)
        (loop-event-handling handle
          :window (on-window-event handle win)
          :quit (leave-event-loop))))

