#Mini-SDL2
SDL2の代表的な機能をCommon Lispから簡単に利用するための補助ライブラリ。  
(SDL2のAPIラッパーではありません)

##Install

###Required
[Common Lisp] CFFI

[OS] SDL2, SDL2\_image, SDL2\_ttf, SDL2\_mixer

##Usage
###General
    with-sdl2 flags &body body
SDL2を使用したプログラムを実行する。  
flagsには、:video :audio :joystick :hapticが有効である。

    set-error &rest fmt
SDLエラーを設定する。  
fmtにはcl:formatに順ずる。

    get-error
SDLエラーを取得する。

    clear-error
SDLエラーをクリアする。

###Event-Handling
    loop-event-handling &optional handle
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

    quit-request
quitイベントをイベントキューに追加する。

    leave-event-loop
loop-event-handlingのループ処理から脱出するフラグを設定する。  
次のイベントポーリング前にループ処理は終了する。

    with-event-slot event-type (handle &rest keys) &body body
イベントハンドラからイベント情報を取得する。  
keysにはevent-typeによって異なるキーワード引数が設定される。
* :window
    * :event
    * :timestamp
    * :window-id
    * :win-event
    * :data1
    * :data2
* :keyboard
    * :event
    * :timestamp
    * :window-id
    * :state
    * :repeat
    * :keysym
* :text-editing
    * :event
    * :timestamp
    * :window-id
    * :text
    * :start
    * :length
* :text-input
    * :event
    * :timestamp
    * :window-id
    * :text
* :mouse-motion
    * :event
    * :timestamp
    * :window-id
    * :state
    * :x
    * :y
    * :xrel
    * :yrel
* :mouse-button
    * :event
    * :timestamp
    * :window-id
    * :button
    * :state
    * :x
    * :y
* :mouse-wheel
    * :event
    * :timestamp
    * :window-id
    * :x
    * :y
* :joy-axis
    * :event
    * :timestamp
    * :which
    * :axis
    * :value
* :joy-ball
    * :event
    * :timestamp
    * :which
    * :ball
    * :xrel
    * :yrel
* :joy-hat
    * :event
    * :timestamp
    * :which
    * :hat
    * :value
* :joy-button
    * :event
    * :timestamp
    * :which
    * :button
    * :state
* :quit
    * :event
    * :timestamp

###Video (Required init with :video)
    with-window (name &key title x y w h flags) &body body
SDL2ウィンドウを生成する。

    with-context name win &body body
SDL2ウィンドウに対応したGL描画コンテキストを生成する。

    with-window-and-context (win ctx &key title x y w h flags) &body body
SDL2ウィンドウとGL描画コンテキストを生成する。

    begin-frame win ctx &body body
描画フレームを開始する。  
フレームの開始時にctxがカレントとして設定され、  
フレームの終了時にwinのバックバッファがスワップされる。

    load-image pathname
pathnameのイメージフォーマットをロードする。  
imageオブジェクトまたは読み込みに失敗した場合はnilが返る。

    close-image image
imageオブジェクトを解放する。  
bind-gltexでバインディングしたgl-textureは自動的に解放されない。

    bind-gltex image
imageをカレントのGLコンテキストにテクスチャとしてバインドする。  
バインド成功した場合、返り値はGLのテクスチャ番号、失敗した場合はnilが返る。  
初回のバインド時にはgen-textureが発生する。

    load-font pathname ptsize index
pathnameのフォント(.ttf)をロードする。  
fontオブジェクトまたは読み込みに失敗した場合はnilが返る。

    font-attribute attr font (&rest value)
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

    close-font font
fontオブジェクトを解放する。

    render-text render-mode font text color
fontからレンダリング結果のimageオブジェクトを生成する。  
render-modeは:solid :shaded :blendedが指定できる。

###Input (Required init with :joystick)
    num-joysticks
認識されているジョイスティックデバイスの個数を取得する。

    joystick-style index
ジョイスティックの構造を取得する。  
返り値はリストで、(name num-axes num-balls num-buttons num-hats)を表す。

    joystick-open index
ジョイスティックを使用開始する。

    joystick-close index
ジョイスティックの使用を終了する。

    joystick-opended index
指定した番号のジョイスティックを使用しているか判定する。

    list-active-joysticks
使用中のジョイスティックIDのリストを取得する。

###Audio (Required init with :audio)
    with-audio (&key freqency format channels chunksize) &body body
オーディオデバイスの使用を開始する。

    load-wave pathname
pathnameの音声ファイルをロードする。  
sampleオブジェクトまたは、音声ファイルの読み込みに失敗した場合はnilが返る。

    close-sample sample
音声ファイルを解放する。

    load-music pathname
pathnameから音楽ファイルをロードする。  
musicオブジェクトまたは読み込みに失敗した場合はnilが返る。

    close-music music
musicオブジェクトを解放する。

    channels count
ミキシングのチャンネル数を設定する。  
返り値は実際に用意できたチャンネル数。

    volume channel-or-music volume
チャンネルのボリュームを設定する。  
channel-or-musicが:musicの場合は、音楽再生のボリュームを設定する。

    play channel-or-music sample-or-music loops ms
チャンネルにサンプルを割り当てて再生する。  
msが指定された場合はフェードイン開始する。  
channel-or-musicが:musicの場合、音楽再生を開始する。

    pause channel-or-music
チャンネルの再生を一時停止する。  
channel-or-musicが:musicの場合、音楽再生を一時停止する。

    resume channel-or-music
チャンネルの再生を再開する。  
channel-or-musicが:musicの場合、音楽再生を一時停止する。

    halt channel-or-music ms
チャンネルの再生を停止する。  
msが指定された場合はフェードアウト終了する。  
channel-or-musicが:musicの場合、音楽再生を停止する。

    playing channel-or-music
チャンネルが再生中か判別する。  
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

