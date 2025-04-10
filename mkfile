EAPP=/Applications/Emacs.app/Contents/MacOS
EDMP=$EAPP/libexec
ESTE=$HOME/.emacs.d

dump:VQ:
 $EAPP/Emacs --dump "${EDMP}/Emacs.pdmp.bak" --batch  --load "${ESTE}/dump.el"
 mv $ESTE/Emacs.pdmp $EDMP/Emacs.pdmp

init:VQ:
 cp $EDMP/Emacs.pdmp $EDMP/Emacs.pdmp.bak

orig:VQ:
 $EAPP/Emacs --dump "${EDMP}/Emacs.pdmp.bak"
