class SpaceAge {

    seconds: number;

    constructor(seconds: number) {
        this.seconds = seconds;
    }

    readonly earthYear: number = 31557600;

    readonly orbitalPeriod: Map<string, number> =
        new Map([
            ['onMercury', 0.2408467],
            ['onVenus', 0.61519726],
            ['onEarth', 1.0],
            ['onMars', 1.8808158],
            ['onJupiter', 11.862615],
            ['onSaturn', 29.447498],
            ['onUranus', 84.016846],
            ['onNeptune', 164.79132]
        ])

    getAge(planet: string): number {
        let x = this.seconds / this.orbitalPeriod.get(planet)!;
        let result = x / this.earthYear;

        return parseFloat(result.toFixed(2));
    }

    onEarth(): number {
        return this.getAge('onEarth');
    }
    onMercury(): number {
        return this.getAge('onMercury');
    }
    onVenus(): number {
        return this.getAge('onVenus');
    }
    onMars(): number {
        return this.getAge('onMars');
    }
    onJupiter(): number {
        return this.getAge('onJupiter');
    }
    onSaturn(): number {
        return this.getAge('onSaturn');
    }
    onUranus(): number {
        return this.getAge('onUranus');
    }
    onNeptune(): number {
        return this.getAge('onNeptune');
    }
}

export default SpaceAge