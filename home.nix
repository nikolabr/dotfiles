{
  config,
  pkgs,
  inputs,
  ...
}:

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
        white = "0x282a2e";
      };
    };
    font.size = 8.0;
    window = {
      opacity = 0.85;
    };
  };

  services.syncthing = {
    enable = true;
  };
  services.lorri = {
    enable = true;
  };

  # Tweak for 23.05, hopefully will be fixed in unstable
  systemd.user.services.lorri.serviceConfig = {
    ProtectSystem = pkgs.lib.mkForce "full";
    ProtectHome = pkgs.lib.mkForce false;
  };

  # programs.rofi.enable = true;
  # programs.rofi.theme = "Arc-Dark";
  # programs.rofi.font = "Noto Sans Mono 12";
  # programs.rofi.terminal = "alacritty";

  # programs.xmobar = {
  #   enable = true;
  #   extraConfig = builtins.readFile ./xmonad/.xmobarrc;
  # };

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
  
  fonts.fontconfig.enable = true;

  xdg.mimeApps =
    let
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
    in
    {
      enable = true;
      associations.added = firefoxAssociations;

      # Merge the firefox associations and custom ones
      defaultApplications = pkgs.lib.attrsets.mergeAttrsList [
        {
          "text/html" = [ "firefox.desktop" ];
        }
        firefoxAssociations
      ];
    };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client.enable = true;

    # package = pkgs.emacs-pgtk;
    package = with pkgs; (
      (emacsPackagesFor emacs-pgtk).emacsWithPackages (
        epkgs: [ epkgs.lsp-bridge ]
      ));
  };

  home.file.".emacs".source = ./files/.emacs;
  home.file.".config/emacs/early-init.el".source = ./files/early-init.el;

  # programs.vscode = {
  #   enable = true;
  #   extensions = with pkgs.vscode-extensions; [
  #     ms-vscode.cpptools
  #     ms-vscode-remote.remote-containers

  #     twxs.cmake
  #     ms-vscode.cmake-tools

  #     tuttieee.emacs-mcx
  #   ];
  # };

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
    inkscape

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
    (aspellWithDicts (d: [
      d.en
      d.sl
    ]))

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
    bear
    devcontainer
    
    iosevka

    # Tex
    texlive.combined.scheme-full

    powershell

    # Octave
    (octaveFull.withPackages (octavePackages: [
      octavePackages.audio
      octavePackages.statistics
      octavePackages.signal
    ]))

    jetbrains.idea-ultimate
    azure-cli

    # Rust
    (rust-bin.stable.latest.default.override {
      extensions = [
        "rust-src"
        "rust-analyzer"
      ];
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

    # Embedded
    gcc-arm-embedded
    qemu_full

    xournalpp

    viber
    signal-desktop
    skypeforlinux
    wine

    usbutils

    simplescreenrecorder

    networkmanagerapplet
    remmina
    openconnect
    networkmanager-openconnect
    
    (rstudioWrapper.override {
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
