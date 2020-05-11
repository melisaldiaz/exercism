class TwoFer {
  public static twoFer(name: string = 'you'): string {

    const message: [string, string] = ['One for ', ', one for me.'];

    return message[0] + name + message[1]

  }
}

export default TwoFer



