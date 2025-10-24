let SDL = ffiOpen({
  version: "1",
  lib: "libSDL2-2.0.so.0",
  types: {
    i32:{kind:"int",bits:32,signed:true},
    u32:{kind:"int",bits:32,signed:false},
    u8:{kind:"int",bits:8,signed:false},

    SDL_Window:{kind:"pointer",to:{kind:"void"},tag:"sdl.window*"},
    SDL_Renderer:{kind:"pointer",to:{kind:"void"},tag:"sdl.renderer*"},
    charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}},

    Rect:{kind:"struct",fields:[
      {name:"x",type:"i32"},
      {name:"y",type:"i32"},
      {name:"w",type:"i32"},
      {name:"h",type:"i32"}
    ]},

    # Oversize to be safe across SDL builds; we only read 'type' (offset 0)
    Event:{kind:"struct",fields:[
      {name:"type",type:"u32"},
      {name:"_pad",type:{kind:"array",of:"u8",len:252}}
    ]}
  },
  functions: [
    {name:"SDL_Init",ret:"i32",params:["u32"]},
    {name:"SDL_Quit",ret:{kind:"void"},params:[]},

    {name:"SDL_CreateWindow",ret:"SDL_Window",
     params:["charp","i32","i32","i32","i32","u32"]},
    {name:"SDL_DestroyWindow",ret:{kind:"void"},params:["SDL_Window"]},

    {name:"SDL_CreateRenderer",ret:"SDL_Renderer",params:["SDL_Window","i32","u32"]},
    {name:"SDL_DestroyRenderer",ret:{kind:"void"},params:["SDL_Renderer"]},

    {name:"SDL_SetRenderDrawColor",ret:"i32",params:["SDL_Renderer","u8","u8","u8","u8"]},
    {name:"SDL_RenderClear",ret:{kind:"void"},params:["SDL_Renderer"]},
    {name:"SDL_RenderFillRect",ret:"i32",params:["SDL_Renderer",{kind:"pointer",to:"Rect"}]},
    {name:"SDL_RenderPresent",ret:{kind:"void"},params:["SDL_Renderer"]},

    {name:"SDL_GetTicks",ret:"u32",params:[]},
    {name:"SDL_PumpEvents",ret:{kind:"void"},params:[]},
    {name:"SDL_PollEvent",ret:"i32",params:[{kind:"pointer",to:"Event"}]}
  ]
})

# ---- Constants ----
let SDL_INIT_VIDEO = 32
let SDL_WINDOWPOS_X = 100
let SDL_WINDOWPOS_Y = 100
let SDL_WINDOW_W = 800
let SDL_WINDOW_H = 600
let SDL_WINDOW_SHOWN = 4
let SDL_RENDERER_ACCELERATED = 2
let SDL_RENDERER_PRESENTVSYNC = 4
let RENDERER_FLAGS = SDL_RENDERER_ACCELERATED + SDL_RENDERER_PRESENTVSYNC

let SDL_QUIT = 256

# ---- Init ----
if SDL.SDL_Init(SDL_INIT_VIDEO) != 0 then fail("SDL_Init failed") end
let win = SDL.SDL_CreateWindow("SDL2 C Demo",
  SDL_WINDOWPOS_X, SDL_WINDOWPOS_Y,
  SDL_WINDOW_W, SDL_WINDOW_H,
  SDL_WINDOW_SHOWN)
if win == null then SDL.SDL_Quit(); fail("SDL_CreateWindow failed") end

# NOTE: renderer index must be -1 to auto-select (matches C)
let ren = SDL.SDL_CreateRenderer(win, -1, RENDERER_FLAGS)
if ren == null then
  SDL.SDL_DestroyWindow(win); SDL.SDL_Quit(); fail("SDL_CreateRenderer failed")
end

# ---- State ----
let ballW = 40
let ballH = 40
let x = 100
let y = 100
let vx = 5
let vy = 4

let rect = SDL.__mem.box("Rect", { x:x, y:y, w:ballW, h:ballH })
let ev = SDL.__mem.box("Event", null)

# Fixed-timestep (Uint32 wrap-safe)
let STEP_MS = 16
let acc = 0
let prev = SDL.SDL_GetTicks()

let running = true
while running do
  # Pump once per frame (Wayland-friendly), then drain
  SDL.SDL_PumpEvents()
  while SDL.SDL_PollEvent(ev) != 0 do
    if SDL.__mem.getf("Event", ev, "type") == SDL_QUIT then
      running = false
    end
  end

  # Advance time with wrap-safe delta (Uint32 behavior)
  let now = SDL.SDL_GetTicks()
  let dt0 = now - prev
  let dt = if dt0 < 0 then dt0 + 4294967296 else dt0 end
  prev = now
  acc = acc + dt

  # Step simulation in fixed increments
  while acc >= STEP_MS do
    x = x + vx
    y = y + vy

    if x < 0 then x = 0; vx = 0 - vx end
    if y < 0 then y = 0; vy = 0 - vy end
    if x + ballW > SDL_WINDOW_W then x = SDL_WINDOW_W - ballW; vx = 0 - vx end
    if y + ballH > SDL_WINDOW_H then y = SDL_WINDOW_H - ballH; vy = 0 - vy end

    acc = acc - STEP_MS
  end

  # Update rect fields
  SDL.__mem.setf("Rect", rect, "x", x)
  SDL.__mem.setf("Rect", rect, "y", y)

  # Render one frame (vsync paces us)
  SDL.SDL_SetRenderDrawColor(ren, 30, 30, 36, 255)
  SDL.SDL_RenderClear(ren)

  SDL.SDL_SetRenderDrawColor(ren, 240, 90, 40, 255)
  SDL.SDL_RenderFillRect(ren, rect)

  SDL.SDL_RenderPresent(ren)

  # No SDL_Delay(): Present with VSYNC already throttles (matches C)
end

# ---- Cleanup ----
SDL.SDL_DestroyRenderer(ren)
SDL.SDL_DestroyWindow(win)
SDL.SDL_Quit()
