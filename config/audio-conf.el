(defconst volumeModifier "4")

(defun audio/mute ()
  "Toggle mute of pulsemixer."
  (interactive)
  (start-process "audio-mute" nil "pulsemixer" "--toggle-mute"))

(defun audio/raise-volume ()
  "Raise the volume by volumeModifier of Pulsemixer."
  (interactive)
  (start-process "raise-volume" nil "pulsemixer" "--change-volume" (concat "+" volumeModifier)))

(defun audio/lower-volume ()
  "Lower the volume by volumeModifier of Pulsemixer."
  (interactive)
  (start-process "lower-volume" nil "pulsemixer" "--change-volume" (concat "-" volumeModifier)))


(global-set-key (kbd "<XF86AudioMute>") 'audio/mute)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'audio/raise-volume)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'audio/lower-volume)
