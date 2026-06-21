import { useEffect, useRef, useState } from "react";

const THRESHOLD = 70; // px pulled (after resistance) to trigger
const MAX = 120;
const RESISTANCE = 0.5;

/**
 * Pull-to-refresh for touch devices: when scrolled to the top, dragging down past
 * the threshold reloads the page with a cache-busting query so fresh assets load.
 */
export function usePullToRefresh() {
  const [distance, setDistance] = useState(0);
  const [refreshing, setRefreshing] = useState(false);
  const startY = useRef<number | null>(null);
  const distRef = useRef(0);
  const refreshingRef = useRef(false);

  useEffect(() => {
    const onStart = (e: TouchEvent) => {
      // The scroll viewport is .app-main (the shell is a fixed-height flex column);
      // fall back to the window for screens without it, such as the auth gate.
      const scroller = document.querySelector(".app-main");
      const atTop = scroller ? scroller.scrollTop <= 0 : window.scrollY <= 0;
      startY.current = atTop && e.touches.length === 1 ? e.touches[0].clientY : null;
    };

    const onMove = (e: TouchEvent) => {
      if (startY.current === null || refreshingRef.current) return;
      const dy = e.touches[0].clientY - startY.current;
      if (dy <= 0) {
        distRef.current = 0;
        setDistance(0);
        return;
      }
      const d = Math.min(MAX, dy * RESISTANCE);
      distRef.current = d;
      setDistance(d);
      e.preventDefault(); // suppress native rubber-band / pull-to-refresh while dragging
    };

    const onEnd = () => {
      if (startY.current === null) return;
      startY.current = null;
      if (distRef.current >= THRESHOLD && !refreshingRef.current) {
        refreshingRef.current = true;
        setRefreshing(true);
        const url = new URL(window.location.href);
        url.searchParams.set("r", Date.now().toString());
        window.location.replace(url.toString());
      } else {
        distRef.current = 0;
        setDistance(0);
      }
    };

    window.addEventListener("touchstart", onStart, { passive: true });
    window.addEventListener("touchmove", onMove, { passive: false });
    window.addEventListener("touchend", onEnd);
    return () => {
      window.removeEventListener("touchstart", onStart);
      window.removeEventListener("touchmove", onMove);
      window.removeEventListener("touchend", onEnd);
    };
  }, []);

  return { distance, threshold: THRESHOLD, refreshing };
}
