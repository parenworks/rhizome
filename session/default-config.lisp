(rhizome:configure (list :bar (list :modules (list :left (list :workspaces :layout :windows) :center (list :clock) :right (list :tray :network :volume :battery :display)) :enabled t :margin 4 :corner-radius 10 :height 28)))
(rhizome:configure (list :modules (list :display (list :on-click "rhizome-cmd display-menu"))))
