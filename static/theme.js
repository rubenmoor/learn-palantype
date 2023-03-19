{
    fontFamily: {
        sans: [ "Abel", "sans-serif"],
        serif: [ "Special Elite", "cursive"],
        mono: [ "DejaVu Sans Mono", "monospace"]
    },
    extend: {
        colors: {
            grayishblue:  {
                "200": "#B9CEFF",
                "300": "#7e85a7",
                "500": "#729CE0",
                "700": "#0059b2",
                "800": "#003366",
                "900": "#081430"
            }
        },
        animation: {
            flashing: 'flash 1s ease infinite'
        },
        keyframes: {
            flash: {
                '0%, 100%': { opacity: 0.5 },
                '50%': { opacity: 1 }
            }
        }
    }
}
