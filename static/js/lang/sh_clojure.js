if (! this.sh_languages) {
  this.sh_languages = {};
}
sh_languages['clojure'] = [
  [
    [
      /#\|/g,
      'sh_comment',
      1
    ],
    [
      /;/g,
      'sh_comment',
      2
    ],
    [
      /"/g,
      'sh_string',
      3
    ],
    [
      /(?:\#(?:x|o|b|X|O|B))?[+-]?(?:(?:0x[A-Fa-f0-9]+)|(?:(?:[\d]*\.)?[\d]+(?:[eE][+-]?[\d]+)?))u?(?:(?:int(?:8|16|32|64))|L)?/g,
      'sh_number',
      -1
    ],
    [
      /#?'(?:\b|\\/|\+|\?|\*|\-)[A-Za-z0-9_\\/\-\+\*\?:]+(?:\+|\?|\*|\-|\\/|\b)/g,
      'sh_type',
      -1
    ],
    [
      /:[A-Za-z0-9\-]+/g,
      'sh_preproc',
      -1
    ],
    [
      /(?:\+|\*)[A-Za-z0-9_\-\+\*\?:]+(?:\+|\*)/g,
      'sh_variable',
      -1
    ],
    [
      /(?:\b|\+|\?|\*|\-)[A-Za-z0-9_\-\+\*\?:]+(?:\+|\?|\*|\-|\b)/g,
      'sh_normal',
      -1
    ],
    [
      /(\()(defonce|def|defvar|and|cond|:?require|:?use|:only|:as|declare|when|defmulti|defmethod|ns|defstruct|fn|defmacro|loop|let\*|let|do|cons|conj|first|last|set\!|get|get-in|if|equal||\+|\*|\-)/g,
      ['sh_cbracket', 'sh_keyword'],
      -1
    ],
    [
      /\(|\)/g,
      'sh_cbracket',
      -1
    ]
  ],
  [
    [
      /\|#/g,
      'sh_comment',
      -2
    ],
    [
      /#\|/g,
      'sh_comment',
      1
    ]
  ],
  [
    [
      /$/g,
      null,
      -2
    ]
  ],
  [
    [
      /"/g,
      'sh_string',
      -2
    ],
    [
      /\\./g,
      'sh_specialchar',
      -1
    ]
  ]
];
