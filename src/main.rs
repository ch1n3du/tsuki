use clap::{Args, Parser, Subcommand};
fn main() {
    let cli = TsukiCli::parse();
    cli.commands.execute();
}

#[derive(Debug, Parser)]
#[command(name = "tsuki")]
#[command(version = "0.0.1")]
#[command(about = "A CLI for the Tsuki programming language")]
struct TsukiCli {
    #[command(subcommand)]
    commands: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    #[command(arg_required_else_help = true)]
    #[command(about = "Compile a '.tsuki' file into a WebAssembly module")]
    Compile {
        #[arg(value_name = "INPUT_FILE")]
        src_file_path: String,
        #[arg(value_name = "OUTPUT_FILE")]
        output_file_path: String,
    },
}

impl Commands {
    fn execute(self) {
        match self {
            Commands::Compile {
                src_file_path,
                output_file_path,
            } => {
                let src = std::fs::read_to_string(&src_file_path).unwrap();
                tsuki::try_to_compile_module_to_file(&src, &output_file_path);
                println!(
                    "Success: Compiled '{}' to '{}'",
                    src_file_path, output_file_path
                );
            }
        }
    }
}
