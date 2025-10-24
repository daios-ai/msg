// sdl_demo.c
#include <SDL.h>
#include <stdio.h>

int main(int argc, char** argv) {
    (void)argc; (void)argv;

    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "SDL_Init failed: %s\n", SDL_GetError());
        return 1;
    }

    const int W = 800, H = 600;
    SDL_Window*   win = SDL_CreateWindow("SDL2 C Demo",
                           100, 100, W, H, SDL_WINDOW_SHOWN);
    if (!win) {
        fprintf(stderr, "SDL_CreateWindow failed: %s\n", SDL_GetError());
        SDL_Quit();
        return 1;
    }

    // Accelerated + vsync renderer
    const Uint32 RENDER_FLAGS = SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC;
    SDL_Renderer* ren = SDL_CreateRenderer(win, -1, RENDER_FLAGS);
    if (!ren) {
        fprintf(stderr, "SDL_CreateRenderer failed: %s\n", SDL_GetError());
        SDL_DestroyWindow(win);
        SDL_Quit();
        return 1;
    }

    // Ball state
    int bw = 40, bh = 40;
    int x = 100, y = 100;
    int vx = 5, vy = 4;

    // Fixed timestep
    const Uint32 STEP_MS = 16; // ~60Hz
    Uint32 acc  = 0;
    Uint32 prev = SDL_GetTicks();

    int running = 1;
    SDL_Event ev;

    while (running) {
        // Pump the queue *every frame* (important on Wayland)
        SDL_PumpEvents();

        // Drain pending events (non-blocking)
        while (SDL_PollEvent(&ev)) {
            if (ev.type == SDL_QUIT) {
                running = 0;
            }
        }

        // Advance time
        Uint32 now = SDL_GetTicks();
        Uint32 dt  = now - prev;  // works across wrap for Uint32
        prev = now;
        acc += dt;

        // Step simulation in fixed increments
        while (acc >= STEP_MS) {
            x += vx;
            y += vy;

            if (x < 0)            { x = 0;            vx = -vx; }
            if (y < 0)            { y = 0;            vy = -vy; }
            if (x + bw > W)       { x = W - bw;       vx = -vx; }
            if (y + bh > H)       { y = H - bh;       vy = -vy; }

            acc -= STEP_MS;
        }

        // Render one frame (vsync paces us)
        SDL_SetRenderDrawColor(ren, 30, 30, 36, 255);
        SDL_RenderClear(ren);

        SDL_Rect r = { x, y, bw, bh };
        SDL_SetRenderDrawColor(ren, 240, 90, 40, 255);
        SDL_RenderFillRect(ren, &r);

        SDL_RenderPresent(ren);

        // No SDL_Delay(): with PRESENTVSYNC, Present already throttles
    }

    SDL_DestroyRenderer(ren);
    SDL_DestroyWindow(win);
    SDL_Quit();
    return 0;
}
