export namespace Elm {
    namespace Main {
        interface App {
            ports: Ports;
        }

        interface Args {
            node: HTMLElement;
            flags: any;
        }

        interface Ports {
            alertString: Subscribe<string>
            alertInt: Subscribe<number>
            alertBool: Subscribe<boolean>
            alertFloat: Subscribe<number>
        }

        interface Subscribe<T> {
            subscribe(callback: (value: T) => any): void;
        }

        interface Send<T> {
            send(value: T): void;
        }

        function init(args: Args): App;
    }
}
