// key_listener.c
// The implementation of our key listener C library with feedback loop
// prevention. This version uses Process IDs (PID) for robust self-event
// detection.

#include "key_listener.h"
#include <ApplicationServices/ApplicationServices.h>
#include <pthread.h> // For running the event loop on a background thread
#include <stdio.h>
#include <unistd.h> // For getpid()

// --- Global State ---
static haskell_key_callback g_callback = NULL;
static CFRunLoopRef g_runLoop = NULL;
// Store our own process ID to identify self-generated events.
static pid_t g_pid;

// This is the C callback that the OS will call.
CGEventRef internal_c_callback(CGEventTapProxy proxy, CGEventType type,
                               CGEventRef event, void *refcon) {
  // --- INFINITE LOOP PREVENTION ---
  // Get the PID of the process that created the event.
  pid_t event_pid =
      (pid_t)CGEventGetIntegerValueField(event, kCGEventSourceUnixProcessID);

  // If the event's PID matches our own, it's an event we created.
  // Ignore it by passing it through immediately.
  if (event_pid == g_pid) {
    return event;
  }

  // If it's not our event, proceed with processing.
  if (type == kCGEventKeyDown || type == kCGEventKeyUp) {
    CGKeyCode keyCode =
        (CGKeyCode)CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);
    if (g_callback != NULL) {
      int swallow_event = g_callback(type, (int)keyCode);
      if (swallow_event == 1) {
        return NULL;
      }
    }
  }
  return event;
}

// This function will run on a separate thread.
void *run_loop_thread_func(void *arg) {
  g_runLoop = CFRunLoopGetCurrent();
  CGEventMask eventMask = (1 << kCGEventKeyDown) | (1 << kCGEventKeyUp);
  CFMachPortRef eventTap =
      CGEventTapCreate(kCGSessionEventTap, kCGHeadInsertEventTap, 0, eventMask,
                       internal_c_callback, NULL);

  if (!eventTap) {
    fprintf(stderr, "[C Lib] ERROR: Failed to create event tap.\n");
    return NULL;
  }

  CFRunLoopSourceRef runLoopSource =
      CFMachPortCreateRunLoopSource(kCFAllocatorDefault, eventTap, 0);
  CFRunLoopAddSource(g_runLoop, runLoopSource, kCFRunLoopDefaultMode);

  CGEventTapEnable(eventTap, true);
  printf("[C Lib] Event loop starting on background thread.\n");
  CFRunLoopRun();
  printf("[C Lib] Event loop stopped.\n");

  CFRelease(eventTap);
  CFRelease(runLoopSource);

  return NULL;
}

// --- Public API Implementation ---

int start_key_listener(haskell_key_callback callback) {
  if (callback == NULL)
    return -1;
  g_callback = callback;

  // Store our own process ID at startup.
  g_pid = getpid();

  pthread_t thread;
  if (pthread_create(&thread, NULL, run_loop_thread_func, NULL) != 0) {
    fprintf(stderr, "[C Lib] ERROR: Failed to create background thread.\n");
    return -1;
  }
  pthread_detach(thread);
  return 0;
}

void stop_key_listener(void) {
  if (g_runLoop != NULL) {
    CFRunLoopStop(g_runLoop);
  }
}

void send_key_event(int key_code, bool is_down) {
  // We no longer need a custom event source. The OS will stamp our PID
  // automatically. Pass NULL for the source.
  CGEventRef event =
      CGEventCreateKeyboardEvent(NULL, (CGKeyCode)key_code, is_down);

  if (event == NULL) {
    fprintf(stderr, "[C Lib] ERROR: Failed to create keyboard event.\n");
    return;
  }

  CGEventPost(kCGHIDEventTap, event);
  CFRelease(event);
}
