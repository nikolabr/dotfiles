{ config, pkgs, inputs,  ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "nikola";
  home.homeDirectory = "/home/nikola";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  programs.bash.enable = true;
  programs.bash.bashrcExtra = "eval \"\$(direnv hook bash)\"";

  home.sessionPath = [
    "$HOME/.npm-global/bin"
  ];
  
  programs.alacritty.enable = false;
  programs.alacritty.settings = {
    colors = {
      primary = {
        background = "0x1d1f21";
        foreground = "0xc5c8c6";
      };
      normal = {
        black = "0x1d1f21";
        red = "0xcc6666";
        green = "0xb5bd68";
        yellow = "0xe6c547";
        blue = "0x81a2be";
        magenta = "0xb294bb";
        cyan = "0x70c0ba";
        white = "0x373b41";
      };
      bright = {
        black = "0x666666";
        red = "0xff3334";
        green = "0x9ec400";
        yellow = "0xf0c674";
        blue = "0x81a2be";
        magenta = "0xb77ee0";
        cyan = "0x54ced6";
        White = "0x282a2e";
      };
    };
    font.size = 8.0;
    window = {
      opacity = 0.85;
    };
  };

  services.syncthing = { enable = true; };
  services.lorri = {
    enable = true;
  };

  # Tweak for 23.05, hopefully will be fixed in unstable
  systemd.user.services.lorri.serviceConfig = {
    ProtectSystem = pkgs.lib.mkForce "full";
    ProtectHome = pkgs.lib.mkForce false;
  };
  
  programs.ssh = {
    enable = true;
    matchBlocks.arnes = {
      host = "*.arnes.si";
      user = "nb91605";
      extraOptions = {
        IdentityFile = "/home/nikola/.ssh/id_sling";
        GSSAPIAuthentication = "yes";
      };
    };
  };

  programs.git = {
    enable = true;
    # Enable send-email
    package = pkgs.gitAndTools.gitFull;

    userEmail = "nikolabrk@protonmail.com";
    userName = "nikolabr";
  };

  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/interface".color-scheme = "prefer-dark";
    };
  };

  gtk = {
    enable = true;
    gtk3 = {
      extraConfig = {
        gtk-application-prefer-dark-theme = "1";
      };
    };
  };

  accounts.email.accounts = {
    "nb91605" = {
      primary = true;
      userName = "nb91605@student.uni-lj.si";
      address = "nb91605@student.uni-lj.si";
      realName = "Nikola Brković";
      flavor = "outlook.office365.com";
      mu.enable = true;
      thunderbird = {
        enable = true;
      };
      offlineimap = {
        enable = true;
        extraConfig.remote = {
          auth_mechanisms = "XOAUTH2";
          # Thunderbird ID
          oauth2_client_id = "9e5f94bc-e8a4-4e73-b8be-63364c29d753";
          
          oauth2_access_token = "";
        };
      };
      imap = {
        host = "outlook.office365.com";
      };
    };
  };

  programs.thunderbird = {
    enable = true;
    profiles = {
      "default" = {
        isDefault = true;
      };
    };
  };

  programs.mu = {
    enable = true;
  };

  programs.offlineimap = {
    enable = true;
  };

  services.sxhkd = {
    enable = false;
    keybindings = {
      # General
      "super + Return" = "alacritty";
      "super + @space" = "rofi -show run";
      "super + v" = "firefox-esr";
      "super + d" = "rofi -show drun";
      "super + Escape" = "pkill -USR1 -x sxhkd";

      # Lock
      "super + ctrl + s" = "systemctl suspend";
      "super + alt + s" = "loginctl lock-session";

      # Volume 
      "XF86AudioRaiseVolume" = "pactl set-sink-volume @DEFAULT_SINK@ +5%";
      "XF86AudioLowerVolume" = "pactl set-sink-volume @DEFAULT_SINK@ -5%";
      "XF86AudioMute" = "pactl set-sink-mute @DEFAULT_SINK@ toggle";

      # Screenshots
      "Print" = "maim ~/Pictures/$(date +%s).png";
      "shift + Print" = "maim -s ~/Pictures/$(date +%s).png";
      "control + Print" = "maim | xclip -selection clipboard -t image/png";
      "shift + control + Print" =
        " maim -s | xclip -selection clipboard -t image/png";

      # Brightness
      "XF86MonBrightnessDown" = "brightnessctl s 10%-";
      "XF86MonBrightnessUp" = "brightnessctl s +10%";

      # BSPWM keybinds

      # quit/restart bspwm
      "super + alt + {q,r}" = "bspc {quit,wm -r}";

      # close and kill
      "super + {_,shift + }w" = "bspc node -{c,k}";

      # alternate between the tiled and monocle layout
      "super + m" = "bspc desktop -l next";

      # send the newest marked node to the newest preselected node
      "super + y" = "bspc node newest.marked.local -n newest.!automatic.local";

      # swap the current node and the biggest window
      "super + g" = "bspc node -s biggest.window";

      # set the window state
      "super + {t,shift + t,s,f}" =
        "bspc node -t {tiled,pseudo_tiled,floating,fullscreen}";

      # set the node flags
      "super + ctrl + {m,x,y,z}" =
        "bspc node -g {marked,locked,sticky,private}";

      # focus the node in the given direction
      "super + {_,shift + }{h,j,k,l}" =
        "bspc node -{f,s} {west,south,north,east}";

      # focus the node for the given path jump
      "super + {p,b,comma,period}" =
        "bspc node -f @{parent,brother,first,second}";

      # focus the next/previous window in the current desktop
      "super + {_,shift + }c" = "bspc node -f {next,prev}.local.!hidden.window";

      # focus the next/previous desktop in the current monitor
      "super + bracket{left,right}" = "bspc desktop -f {prev,next}.local";

      # focus the last node/desktop
      "super + {grave,Tab}" = "bspc {node,desktop} -f last";

      # focus the older or newer node in the focus history
      "super + {o,i}" =
        "bspc wm -h off; bspc node {older,newer} -f; bspc wm -h on";

      # focus or send to the given desktop
      "super + {_,shift + }{1-9,0}" = "bspc {desktop -f,node -d} '^{1-9,10}'";

      # preselect the direction
      "super + ctrl + {h,j,k,l}" = "bspc node -p {west,south,north,east}";

      # preselect the ratio
      "super + ctrl + {1-9}" = "bspc node -o 0.{1-9}";

      # cancel the preselection for the focused node
      "super + ctrl + space" = "bspc node -p cancel";

      # cancel the preselection for the focused desktop
      "super + ctrl + shift + space" =
        "bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel";

      # expand a window by moving one of its side outward
      "super + alt + {h,j,k,l}" =
        "bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}";

      # contract a window by moving one of its side inward
      "super + alt + shift + {h,j,k,l}" =
        "bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}";

      # move a floating window
      "super + {Left,Down,Up,Right}" = "bspc node -v {-20 0,0 20,0 -20,20 0}";

    };
  };

  fonts.fontconfig.enable = true;

  xdg.mimeApps = let
    firefoxAssociations = {
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/chrome" = [ "firefox.desktop" ];
      "application/x-extension-htm" = [ "firefox.desktop" ];
      "application/x-extension-html" = [ "firefox.desktop" ];
      "application/x-extension-shtml" = [ "firefox.desktop" ];

      "application/xhtml+xml" = [ "firefox.desktop" ];
      "application/x-extension-xhtml" = [ "firefox.desktop" ];
      "application/x-extension-xht" = [ "firefox.desktop" ];
    };
  in {
    enable = true;
    associations.added = firefoxAssociations;

    # Merge the firefox associations and custom ones
    defaultApplications = pkgs.lib.attrsets.mergeAttrsList [
      {
        "application/pdf" = [ "emacsclient.desktop" ];
        "text/html" = [ "firefox.desktop" ];
      }
      firefoxAssociations
    ];
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client.enable = true;

    package = pkgs.emacs-pgtk;
  };

  home.file.".emacs".source = ./files/.emacs;
  home.file.".config/emacs/early-init.el".source = ./files/early-init.el;

  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      ms-vscode.cpptools
      ms-vscode-remote.remote-containers
      
      twxs.cmake
      ms-vscode.cmake-tools
      
      tuttieee.emacs-mcx
    ];
  };

  programs.tmux = {
    enable = true;
  };
  
  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # Graphical browsers / Clients
    firefox-esr

    # Graphical utils
    pavucontrol
    neofetch
    anki
    gucharmap
    sxiv
    ghidra-bin
    gimp
    komikku
    audacity

    # Terminal utils
    maim
    p7zip
    zip
    aria2
    qbittorrent
    time
    unzip
    flashrom
    tree
    imhex

    clang-tools

    # PDF/Office
    libreoffice
    ghostscript
    poppler_utils

    # Dicts
    # Slovenian
    (aspellWithDicts (d: [ d.en d.sl ]))

    # Xorg
    xclip
    xorg.xinput
    xorg.xev

    # Fonts
    source-code-pro
    font-awesome
    noto-fonts
    noto-fonts-cjk
    hanazono

    # Dev
    stm32cubemx
    stlink-gui
    bear
    devcontainer

	  iosevka

    # Tex
    texlive.combined.scheme-full

    powershell
    arandr
    
    # Octave
    (octaveFull.withPackages (octavePackages:
      [ octavePackages.audio octavePackages.statistics octavePackages.signal ]
    ))
    
    jetbrains.idea-ultimate
    azure-cli

    # Rust
    (rust-bin.stable.latest.default.override {
      extensions = [ "rust-src" "rust-analyzer" ];
      targets = [
        "thumbv6m-none-eabi"
        "thumbv7m-none-eabi"
        "thumbv7em-none-eabi"
        "thumbv7em-none-eabihf"
      ];
    })
    cargo-generate

    # Python
    nodePackages_latest.pyright

    # Lisp
    sbcl

    # Racket
    racket

    # Clojure
    clojure
    clojure-lsp
    leiningen

    # Elm
    elmPackages.elm
    elmPackages.elm-language-server

    # OCaml
    ocamlPackages.merlin

    # Haskell
    haskell-language-server

    # Embedded
    gcc-arm-embedded
    qemu_full
    
    jflap

    viber
    signal-desktop
    skypeforlinux
    wine

    usbutils

    simplescreenrecorder

    matlab

    networkmanagerapplet
    remmina
    openconnect
    networkmanager-openconnect

    (rstudioWrapper.override{
      packages = with rPackages; [
        readxl
        dplyr
        ggplot2
        treemapify
        DiagrammeR
        DiagrammeRsvg
      ];
    })
    
    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/nikola/etc/profile.d/hm--vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
    BROWSER = "firefox-esr";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
