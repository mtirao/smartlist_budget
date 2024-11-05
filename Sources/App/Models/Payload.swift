//
//  Payload.swift
//  smartlist_budget
//
//  Created by Marcos Tirao on 04/11/2024.
//

import JWT

struct Payload: JWTPayload {
    var user: String
    var exp: ExpirationClaim

    func verify(using key: some JWTAlgorithm) throws {
        try self.exp.verifyNotExpired()
    }
}
