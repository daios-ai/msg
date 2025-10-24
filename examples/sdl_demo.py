# sdl_demo.py
import sys
import ctypes
import sdl2

RENDER_FLAGS_TRY = [
    (sdl2.SDL_RENDERER_ACCELERATED | sdl2.SDL_RENDERER_PRESENTVSYNC, "accelerated+vsync"),
    (sdl2.SDL_RENDERER_ACCELERATED,                                 "accelerated"),
    (0,                                                             "any/default"),
    (sdl2.SDL_RENDERER_SOFTWARE,                                    "software"),
]

def create_renderer_with_fallback(win):
    err_last = None
    for flags, label in RENDER_FLAGS_TRY:
        ren = sdl2.SDL_CreateRenderer(win, -1, flags)
        if ren:
            # Optional: print selected backend
            info = sdl2.SDL_RendererInfo()
            sdl2.SDL_GetRendererInfo(ren, ctypes.byref(info))
            name = info.name.decode() if info.name else label
            print(f"[SDL] Using renderer: {name} ({label})")
            return ren
        err_last = sdl2.SDL_GetError().decode("utf-8", "ignore")
    raise RuntimeError(f"SDL_CreateRenderer failed after fallbacks: {err_last}")

def main() -> int:
    if sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO) != 0:
        err = sdl2.SDL_GetError().decode("utf-8", "ignore")
        sys.stderr.write(f"SDL_Init failed: {err}\n")
        return 1

    W, H = 800, 600
    win = sdl2.SDL_CreateWindow(b"SDL2 C Demo", 100, 100, W, H, sdl2.SDL_WINDOW_SHOWN)
    if not win:
        err = sdl2.SDL_GetError().decode("utf-8", "ignore")
        sys.stderr.write(f"SDL_CreateWindow failed: {err}\n")
        sdl2.SDL_Quit()
        return 1

    try:
        ren = create_renderer_with_fallback(win)
    except Exception as e:
        sys.stderr.write(str(e) + "\n")
        sdl2.SDL_DestroyWindow(win)
        sdl2.SDL_Quit()
        return 1

    # Ball state
    bw, bh = 40, 40
    x, y = 100, 100
    vx, vy = 5, 4

    # Fixed timestep (~60Hz)
    STEP_MS = 16
    acc = 0
    prev = sdl2.SDL_GetTicks()

    running = True
    ev = sdl2.SDL_Event()

    while running:
        sdl2.SDL_PumpEvents()
        while sdl2.SDL_PollEvent(ctypes.byref(ev)) != 0:
            if ev.type == sdl2.SDL_QUIT:
                running = False

        now = sdl2.SDL_GetTicks()
        dt = (now - prev) & 0xFFFFFFFF  # emulate Uint32 wrap behavior
        prev = now
        acc += dt

        while acc >= STEP_MS:
            x += vx; y += vy
            if x < 0:       x, vx = 0,        -vx
            if y < 0:       y, vy = 0,        -vy
            if x + bw > W:  x, vx = W - bw,   -vx
            if y + bh > H:  y, vy = H - bh,   -vy
            acc -= STEP_MS

        sdl2.SDL_SetRenderDrawColor(ren, 30, 30, 36, 255)
        sdl2.SDL_RenderClear(ren)
        r = sdl2.SDL_Rect(x, y, bw, bh)
        sdl2.SDL_SetRenderDrawColor(ren, 240, 90, 40, 255)
        sdl2.SDL_RenderFillRect(ren, r)
        sdl2.SDL_RenderPresent(ren)

    sdl2.SDL_DestroyRenderer(ren)
    sdl2.SDL_DestroyWindow(win)
    sdl2.SDL_Quit()
    return 0

if __name__ == "__main__":
    sys.exit(main())
