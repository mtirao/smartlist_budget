//
//  AuthenticateMiddleware.swift
//  smartlist_budget
//
//  Created by Marcos Tirao on 04/11/2024.
//

import Vapor
import JWT

struct AuthenticateMiddleware: AsyncMiddleware {
    func respond(to request: Request, chainingTo next: AsyncResponder) async throws -> Response {
        let payload = try await request.jwt.verify(as: Payload.self)
        print(payload)
        request.parameters.set("userID", to: payload.user)
        return try await next.respond(to: request)
    }
}
