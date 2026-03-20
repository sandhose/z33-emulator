import type * as React from "react";
import { useCallback, useEffect, useRef, useState } from "react";

type FileDropResult = {
  /** Whether a file drag is happening anywhere in the window */
  isWindowDragging: boolean;
  /** Whether the drag is specifically over the drop zone */
  isOverDropZone: boolean;
  /** Ref to attach to the drop zone element */
  dropZoneRef: React.RefObject<HTMLDivElement | null>;
  /** Props to spread on the drop zone element */
  dropZoneProps: {
    onDragOver: (e: React.DragEvent) => void;
    onDragEnter: (e: React.DragEvent) => void;
    onDragLeave: (e: React.DragEvent) => void;
    onDrop: (e: React.DragEvent) => void;
  };
};

/**
 * Manages file drag-and-drop detection at both window and element level.
 *
 * Window-level detection uses dragover as a heartbeat: dragover fires
 * continuously (~50-350ms) while a drag is inside the viewport. We show
 * the overlay on dragenter and schedule a dismiss timeout on every
 * dragover; as long as events keep coming the timeout resets. When the
 * drag leaves the window dragover stops and the timeout fires. This
 * avoids all the dragleave/relatedTarget edge-cases.
 */
export function useFileDrop(onDrop: (files: FileList) => void): FileDropResult {
  const [isWindowDragging, setIsWindowDragging] = useState(false);
  const [isOverDropZone, setIsOverDropZone] = useState(false);
  const dropZoneRef = useRef<HTMLDivElement>(null);
  // oxlint-disable-next-line unicorn/no-useless-undefined -- useRef requires an argument
  const dismissTimeoutRef = useRef<ReturnType<typeof setTimeout>>(undefined);

  useEffect(() => {
    const scheduleDismiss = () => {
      clearTimeout(dismissTimeoutRef.current);
      dismissTimeoutRef.current = setTimeout(() => {
        setIsWindowDragging(false);
        setIsOverDropZone(false);
      }, 500);
    };

    const handleDragEnter = (e: DragEvent) => {
      e.preventDefault();
      if (e.dataTransfer?.types.includes("Files")) {
        setIsWindowDragging(true);
        scheduleDismiss();
      }
    };
    const handleDragOver = (e: DragEvent) => {
      e.preventDefault();
      if (e.dataTransfer?.types.includes("Files")) scheduleDismiss();
    };
    const handleDrop = (e: DragEvent) => {
      e.preventDefault();
      clearTimeout(dismissTimeoutRef.current);
      setIsWindowDragging(false);
      setIsOverDropZone(false);
    };

    window.addEventListener("dragenter", handleDragEnter);
    window.addEventListener("dragover", handleDragOver);
    window.addEventListener("drop", handleDrop);
    return () => {
      clearTimeout(dismissTimeoutRef.current);
      window.removeEventListener("dragenter", handleDragEnter);
      window.removeEventListener("dragover", handleDragOver);
      window.removeEventListener("drop", handleDrop);
    };
  }, []);

  const dropZoneProps = {
    onDragOver: useCallback((e: React.DragEvent) => {
      e.preventDefault();
    }, []),
    onDragEnter: useCallback((e: React.DragEvent) => {
      e.preventDefault();
      if (e.dataTransfer.types.includes("Files")) setIsOverDropZone(true);
    }, []),
    onDragLeave: useCallback((e: React.DragEvent) => {
      e.preventDefault();
      if (
        !(e.relatedTarget instanceof Node) ||
        !dropZoneRef.current?.contains(e.relatedTarget)
      ) {
        setIsOverDropZone(false);
      }
    }, []),
    onDrop: useCallback(
      (e: React.DragEvent) => {
        e.preventDefault();
        e.stopPropagation();
        setIsWindowDragging(false);
        setIsOverDropZone(false);
        if (e.dataTransfer.files.length > 0) onDrop(e.dataTransfer.files);
      },
      [onDrop],
    ),
  };

  return { isWindowDragging, isOverDropZone, dropZoneRef, dropZoneProps };
}
