

# fdroid.el

`fdroid.el` is an Emacs interface to [F-Droid](https://f-droid.org/). Its purpose is to aid in the management of F-Droid packages for an Android device or an emulator inside the comfort of Emacs.  

To install it manually, simply point to the `fdroid.el` Git checkout in your `load-path`.  

    (add-to-list 'load-path "path/to/fdroid.el")

If you'd like to contribute to the project, the easiest way is for you to install the [GNU Guix](https://guix.gnu.org/) package manager and start developing on the local checkout by invoking the following commands:  

    cd /path/to/fdroid.el
    guix shell --pure

Since this package leverages the [fdroidcl](https://github.com/mvdan/fdroidcl) F-Droid desktop client for most of its functionality, it's necessary for you to have it installed. If it's your first time using `fdroidcl`, ensure to download the F-Droid repository index via the `M-x fdroid-update` command.  

An example configuration might look like this:  

    (define-key global-map (kbd "C-c C--") 'fdroid-map) ; Set your preferred binding for the fdroid-map
    (with-eval-after-load 'fdroid
      (setopt fdroid-log-events t) ; Whether messages should be logged after an operation
      (setopt fdroid-sans-device t)) ; Perform `fdroid-*' operations without a connected device

Above, we set a global binding for the `fdroid-map`. From here onward we can use the `fdroid-list-packages` command to show all packages available for the current F-Droid repository. You can then invoke the interactive commands set below or, if you have the [Embark](https://github.com/oantolin/embark) package installed, the corresponding Embark actions on the mini-buffer targets:  

-   **`fdroid-install` (`i`):** install the current package.
-   **`fdroid-uninstall` (`u`):** uninstall the current package.
-   **`fdroid-download` (`d`):** download the current package.
-   **`fdroid-show` (`s`):** show more information about the current package.

You can send feedback, patches, or bug reports to [public@mianmoreno.com](mailto:public@mianmoreno.com).  

