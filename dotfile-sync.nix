{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.dotfile-sync;
in {
  ###### interface
  options = {
    services.dotfile-sync = {
      enable = mkEnableOption "dotfile-sync";

      dotfilesDir = mkOption {
        type = types.path;
        default = null;
        description = ''
          Directory where the dotfiles are to be synced to/from. Must be a git repository.
        '';
      };
    };
  };

  ###### implementation
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.dotfile-sync ];

    systemd.user.services.dotfile-sync = {
      description = "dotfile-sync service";
      wantedBy = [ "default.target" ];

      serviceConfig.Restart = "always";

      script = ''
        exec ${pkgs.dotfile-sync}/bin/dotfile-sync -d ${cfg.dotfilesDir}
      '';
    };
  };
}
