//
//  Basket.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Fluent
import Foundation

enum Status: Codable, CaseIterable {
    case new
    case inprogress
    case delivered
    case closed
}

final class Basket: Model, @unchecked Sendable {
    static let schema = "baskets"
    
    @ID(key: .id)
    var id: UUID?
    
    @Field(key: "date")
    var date: Date
    
    @Field(key: "status")
    var status: Status
    
    @Field(key: "tender_id")
    var tenderId: UUID
    
    @Field(key: "user_id")
    var userId: String

    init() { }

    init(id: UUID? = nil, date: Date, status: Status, userId: String, tenderId: UUID) {
        self.id = id
        self.date = date
        self.status = status
        self.userId = userId
        self.tenderId = tenderId
    }
    
    func toDTO() -> BasketDTO {
        .init(
            id: self.id,
            date: self.$date.value,
            status: self.$status.value)
    }
}
