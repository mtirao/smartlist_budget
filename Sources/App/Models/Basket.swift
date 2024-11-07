//
//  Basket.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Fluent
import Foundation

enum Status: String, Codable, CaseIterable {
    case new
    case inprogress
    case delivered
    case closed
}

final class Basket: Model, @unchecked Sendable {
    static let schema = "baskets"
    
    @ID(key: .id)
    var id: UUID?
    
    
    @Enum(key: "status")
    var status: Status
    
    @Field(key: "tender_id")
    var tenderId: UUID?
    
    @Field(key: "user_id")
    var userId: String

    init() { }

    init(id: UUID? = nil, status: Status, userId: String, tenderId: UUID) {
        self.id = id
        self.status = status
        self.userId = userId
        self.tenderId = tenderId
    }
    
    func toDTO() -> BasketDTO {
        .init(
            id: self.id,
            status: self.$status.value,
            tenderId: self.tenderId)
    }
}
