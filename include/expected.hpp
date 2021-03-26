//
// Created by Riley Quinn on 7/22/20.
//

#ifndef EXPECTED_EXPECTED_HPP
#define EXPECTED_EXPECTED_HPP

#include <utility>
#include <type_traits>
#include <exception>

// Synopsis

template<typename T, typename E>
class expected;

template<typename E>
class unexpected;

template<typename E>
unexpected(E) -> unexpected<E>;

template<typename E>
class bad_expected_access;

template<>
class bad_expected_access<void>;

struct unexpect_t {
};

inline constexpr unexpect_t unexpect{};

// Implementation

template<typename T, typename E>
class expected {
public:
    using value_type = T;
    using error_type = E;
    using unexpected_type = unexpected<E>;

    template<typename U>
    using rebind = expected<U, error_type>;

    constexpr expected() requires(std::is_default_constructible_v<T>): value_(), has_value_(true) {};

    constexpr expected(const expected &rhs) requires (
            std::is_copy_constructible_v<T> &&
            std::is_copy_constructible_v<E>
    ) {
        if ((has_value_ = rhs.has_value_)) {
            value_ = rhs.value_;
        } else {
            unexpected_ = rhs.error();
        }
    }

    constexpr expected(expected &&rhs)
    noexcept(std::is_nothrow_move_constructible_v<T> &&
             std::is_nothrow_move_constructible_v<E>) requires (
            std::is_move_constructible_v<T> &&
            std::is_move_constructible_v<E>
    ) {
        if ((has_value_ = rhs.has_value_)) {
            value_ = std::move(rhs.value_);
        } else {
            unexpected_ = unexpected(std::move(rhs.error()));
        }
    }

    template<typename U, typename G>
    explicit(!std::is_convertible_v<const U &, T> || !std::is_convertible_v<const G &, E>)
    constexpr expected(const expected<U, G> &rhs) requires (
            std::is_constructible_v<T, const U &> &&
            !std::is_constructible_v<T, expected<U, G> &> &&
            !std::is_constructible_v<T, expected<U, G> &&> &&
            !std::is_constructible_v<T, const expected<U, G> &> &&
            !std::is_constructible_v<T, const expected<U, G> &&> &&
            !std::is_convertible_v<expected<U, G> &, T> &&
            !std::is_convertible_v<expected<U, G> &&, T> &&
            !std::is_convertible_v<const expected<U, G> &, T> &&
            !std::is_convertible_v<const expected<U, G> &&, T> &&
            std::is_constructible_v<E, const G &> &&
            !std::is_constructible_v<unexpected<E>, expected<U, G> &> &&
            !std::is_constructible_v<unexpected<E>, expected<U, G> &&> &&
            !std::is_constructible_v<unexpected<E>, const expected<U, G> &> &&
            !std::is_constructible_v<unexpected<E>, const expected<U, G> &&> &&
            !std::is_convertible_v<expected<U, G> &, unexpected<E>> &&
            !std::is_convertible_v<expected<U, G> &&, unexpected<E>> &&
            !std::is_convertible_v<const expected<U, G> &, unexpected<E>> &&
            !std::is_convertible_v<const expected<U, G> &&, unexpected<E>>
    ) {
        if ((has_value_ = rhs.has_value_)) {
            value_ = rhs.value_;
        } else {
            unexpected_ = unexpected(rhs.error());
        }
    }

    template<typename U, typename G>
    explicit(!std::is_convertible_v<U &&, T> && !std::is_convertible_v<G &&, E>)
    constexpr expected(const expected<U, G> &&rhs) requires (
            std::is_constructible_v<T, U &&> &&
            !std::is_constructible_v<T, expected<U, G> &> &&
            !std::is_constructible_v<T, expected<U, G> &&> &&
            !std::is_constructible_v<T, const expected<U, G> &> &&
            !std::is_constructible_v<T, const expected<U, G> &&> &&
            !std::is_convertible_v<expected<U, G> &, T> &&
            !std::is_convertible_v<expected<U, G> &&, T> &&
            !std::is_convertible_v<const expected<U, G> &, T> &&
            !std::is_convertible_v<const expected<U, G> &&, T> &&
            std::is_constructible_v<E, G &&> &&
            !std::is_constructible_v<unexpected<E>, expected<U, G> &> &&
            !std::is_constructible_v<unexpected<E>, expected<U, G> &&> &&
            !std::is_constructible_v<unexpected<E>, const expected<U, G> &> &&
            !std::is_constructible_v<unexpected<E>, const expected<U, G> &&> &&
            !std::is_convertible_v<expected<U, G> &, unexpected<E>> &&
            !std::is_convertible_v<expected<U, G> &&, unexpected<E>> &&
            !std::is_convertible_v<const expected<U, G> &, unexpected<E>> &&
            !std::is_convertible_v<const expected<U, G> &&, unexpected<E>>
    ) {
        if ((has_value_ = rhs.has_value_)) {
            value_ = std::move(rhs.value_);
        } else {
            unexpected_ = unexpected(std::move(rhs.error()));
        }
    }

    template<typename U = T>
    explicit(!std::is_convertible_v<U &&, T>) constexpr expected(U &&v) requires (
            std::is_constructible_v<T, U &&> &&
            !std::is_same_v<std::remove_cvref_t<U>, std::in_place_t> &&
            !std::is_same_v<expected<T, E>, std::remove_cvref_t<U>> &&
            !std::is_same_v<unexpected<E>, std::remove_cvref_t<U>>
    ) : has_value_(true), value_(std::forward<U>(v)) {}

    template<typename G = E>
    explicit(!std::is_convertible_v<const G &, E>) constexpr expected(const unexpected<G> &e) requires(
            std::is_constructible_v<E, const G &>
    ) : has_value_(false), unexpected_(e) {}

    template<typename G = E>
    explicit(!std::is_convertible_v<G &&, E>) constexpr expected(unexpected<G> &&e)
    noexcept(std::is_nothrow_constructible_v<E, G &&>) requires(std::is_constructible_v<E, G &&>)
            : has_value_(false), unexpected_(std::move(e)) {}

    template<typename ...Args>
    constexpr explicit expected(std::in_place_t, Args &&...args) requires(std::is_constructible_v<T, Args...>)
            : has_value_(true), value_(std::forward<Args>(args)...) {}

    template<typename U, typename ...Args>
    constexpr explicit expected(std::in_place_t, std::initializer_list<U> list, Args &&... args) requires(
            std::is_constructible_v<T, std::initializer_list<U> &, Args...>
    ) : has_value_(true), value_(list, std::forward<Args>(args)...) {}

    template<typename ...Args>
    constexpr explicit expected(unexpect_t, Args &&...args) requires(std::is_constructible_v<E, Args...>)
            : has_value_(false), unexpected_(unexpected<E>(std::forward<Args>(args)...)) {}

    template<typename U, typename ...Args>
    constexpr explicit expected(unexpect_t, std::initializer_list<U> list, Args &&...args) requires(
            std::is_constructible_v<E, std::initializer_list<U> &, Args...>
    ) : has_value_(false), unexpected_(unexpected<E>(list, std::forward<Args>(args)...)) {}

    ~expected() {
        if (!std::is_trivially_destructible_v<T> && has_value_) {
            value_.~T();
        } else if (!std::is_trivially_destructible_v<E> && !has_value_) {
            unexpected_.~unexpected<E>();
        }
    }

    expected &operator=(const expected &rhs) noexcept(std::is_nothrow_copy_assignable_v<T>
                                                      && std::is_nothrow_copy_constructible_v<T>) requires (
            std::is_copy_assignable_v<T> &&
            std::is_copy_constructible_v<T> &&
            std::is_copy_assignable_v<E> &&
            std::is_copy_constructible_v<E> &&
            (std::is_nothrow_move_constructible_v<E> || std::is_nothrow_move_constructible_v<T>)) {
        if (rhs) {
            if (has_value_) {
                value_ = rhs.value_;
            } else {
                if constexpr (std::is_nothrow_copy_constructible_v<T>) {
                    unexpected_.~unexpected<E>();
                    value_ = T(rhs.value_);
                } else if constexpr (std::is_nothrow_move_constructible_v<T>) {
                    T tmp = rhs.value_;
                    unexpected_.~unexpected<E>();
                    value_ = std::move(rhs.value_);
                } else {
                    unexpected<E> tmp = unexpected(this->error());
                    unexpected_.~unexpected<E>();
                    try {
                        value_ = T(rhs.value_);
                    } catch (...) {
                        unexpected_ = std::move(tmp);
                        throw;
                    }
                }
            }
        } else {
            if (has_value_) {
                if constexpr (std::is_nothrow_copy_constructible_v<E>) {
                    value_.~T();
                    unexpected_ = unexpected<E>(rhs.error());
                } else if constexpr (std::is_nothrow_move_constructible_v<E>) {
                    auto tmp = unexpected<E>(rhs.error());
                    value_.~T();
                    unexpected_ = std::move(tmp);
                } else {
                    T tmp = *this;
                    value_.~T();
                    try {
                        unexpected_ = unexpected<E>(rhs.error());
                    } catch (...) {
                        value_ = T(std::move(tmp));
                        throw;
                    }
                }
            }
        }
        has_value_ = rhs.has_value_;

        return *this;
    }

    expected &operator=(expected &&rhs) noexcept(std::is_nothrow_move_assignable_v<T> &&
                                                 std::is_nothrow_move_constructible_v<T>) requires (
            std::is_move_constructible_v<T> &&
            std::is_move_assignable_v<T> &&
            std::is_nothrow_move_constructible_v<E> &&
            std::is_nothrow_move_assignable_v<E>) {
        if (rhs) {
            if (has_value_) {
                value_ = std::move(rhs).value_;
            } else {
                if constexpr (std::is_nothrow_move_constructible_v<T>) {
                    unexpected_.unexpected<E>();
                    value_ = T(std::move(rhs).value_);
                } else {
                    auto tmp = unexpected<E>(std::move(this->error()));
                    unexpected_.~unexpected<E>();
                    try {
                        value_ = T(std::move(rhs).value_);
                    } catch (...) {
                        unexpected_ = std::move(tmp);
                        throw;
                    }
                }
            }
        } else {
            if (*this) {
                if constexpr (std::is_nothrow_move_constructible_v<E>) {
                    value_.~T();
                    unexpected_ = unexpected(std::move(rhs.error()));
                } else {
                    T tmp = std::move(value_);
                    value_.~T();
                    try {
                        unexpected_ = unexpected(std::move(rhs.error()));
                    } catch (...) {
                        value_ = std::move(tmp);
                        throw;
                    }
                }
            }
        }
        has_value_ = rhs.has_value_;
        return *this;
    }

    template<typename U = T>
    expected &operator=(U &&v) requires (
            !std::is_same_v<expected<T, E>, std::remove_cvref_t<U>> &&
            !std::conjunction_v<std::is_scalar<T>, std::is_same<T, std::decay_t<U>>> &&
            std::is_constructible_v<T, U> &&
            std::is_assignable_v<T &, U> &&
            std::is_nothrow_move_constructible_v<E>
    ) {
        if (has_value_) {
            value_ = T(std::forward<U>(v));
        } else {
            if constexpr (std::is_nothrow_constructible_v<T, U>) {
                unexpected_.~unexpected<E>();
                value_ = T(std::forward<U>(v));
            } else {
                unexpected<E> tmp = unexpected(std::move(this->error()));
                unexpected_.~unexpected<E>();
                try {
                    value_ = T(std::forward<U>(v));
                } catch (...) {
                    value_ = std::move(tmp);
                    throw;
                }
            }
            has_value_ = true;
        }
        return *this;
    }

    template<typename G = E>
    expected &operator=(const unexpected<G> &e) requires (
            std::is_nothrow_copy_constructible_v<E> &&
            std::is_copy_assignable_v<E>
    ) {
        if (has_value_) {
            value_.~T();
            unexpected_ = unexpected(e.error());
            has_value_ = false;
        } else {
            unexpected_ = unexpected(e.error());
        }
        return *this;
    }

    template<typename G = E>
    expected &operator=(unexpected<G> &&e) requires (
            std::is_nothrow_move_constructible_v<E> &&
            std::is_move_assignable_v<E>
    ) {
        if (has_value_) {
            value_.~T();
            unexpected_ = unexpected(std::move(e.error()));
            has_value_ = false;
        } else {
            unexpected_ = unexpected(std::move(e.error()));
        }
        return *this;
    }

    template<typename ...Args>
    T &emplace(Args &&...args) requires(std::is_nothrow_constructible_v<T, Args...>) {
        if (has_value_) {
            value_ = T(std::forward<Args>(args)...);
        } else if constexpr (std::is_nothrow_constructible_v<T, Args...>) {
            unexpected_.~unexpected<E>();
            value_ = T(std::forward<Args>(args)...);
        } else if constexpr (std::is_nothrow_move_constructible_v<T>) {
            auto tmp = T(std::forward<Args>(args)...);
            unexpected_.~unexpected<E>();
            value_ = T(std::move(tmp));
        } else {
            unexpected<E> tmp = unexpected(std::move(this->error()));
            unexpected_.~unexpected<E>();
            try {
                value_ = T(std::forward<Args>(args)...);
            } catch (...) {
                unexpected_ = std::move(tmp);
                throw;
            }
        }
        has_value_ = true;
        return value_;
    }

    template<typename U, typename ...Args>
    T &emplace(std::initializer_list<U> list, Args &&...args) requires(
            std::is_nothrow_constructible_v<T, std::initializer_list<U> &, Args...>
    ) {
        if (has_value_) {
            value_ = T(list, std::forward<Args>(args)...);
        } else if constexpr (std::is_nothrow_constructible_v<T, std::initializer_list<U> &, Args...>) {
            unexpected_.~unexpected<E>();
            value_ = T(list, std::forward<Args>(args)...);
        } else if constexpr (std::is_nothrow_move_constructible_v<T>) {
            auto tmp = T(list, std::forward<Args>(args)...);
            unexpected_.~unexpected<E>();
            value_ = T(std::move(tmp));
        } else {
            auto tmp = unexpected(std::move(this->error()));
            unexpected_.~unexpected<E>();
            try {
                value_ = T(list, std::forward<Args>(args)...);
            } catch (...) {
                unexpected_ = std::move(tmp);
                throw;
            }
        }
        has_value_ = true;
        return value_;
    }

    void swap(expected<T, E> &rhs)
    noexcept(std::is_nothrow_move_constructible_v<T> && std::is_nothrow_swappable_v<T> &&
             std::is_nothrow_move_constructible_v<E> && std::is_nothrow_swappable_v<E>) requires (
            std::is_swappable_v<T> &&
            std::is_swappable_v<E> &&
            (std::is_move_constructible_v<T> || std::is_move_constructible_v<E>)
    ) {
        if (rhs.has_value_) {
            if (has_value_) {
                std::swap(value_, rhs.value_);
            } else {
                rhs.swap(*this);
            }
        } else {
            if (has_value_) {
                if constexpr (std::is_nothrow_move_constructible_v<E>) {
                    auto tmp = unexpected<E>(std::move(rhs.unexpected_));
                    rhs.unexpected_.~unexpected<E>();
                    try {
                        rhs.value_ = std::move(*this);
                    } catch (...) {
                        rhs.unexpected_ = std::move(tmp);
                        throw;
                    }
                    this->value_.~T();
                    unexpected_ = std::move(tmp);
                    has_value_ = false;
                    rhs.has_value_ = true;
                } else if constexpr (std::is_nothrow_move_constructible_v<T>) {
                    auto tmp = T(std::move(value_));
                    value_.~T();
                    try {
                        unexpected_ = std::move(rhs.error());
                    } catch (...) {
                        value_ = std::move(tmp);
                        throw;
                    }
                    rhs.unexpected_.~unexpected<E>();
                    rhs.value_ = std::move(tmp);
                    has_value_ = false;
                    rhs.has_value_ = true;
                }
            } else {
                std::swap(unexpected_, rhs.unexpected_);
            }
        }
    }

    constexpr const T *operator->() const {
        return std::addressof(value_);
    }

    constexpr T *operator->() {
        return std::addressof(value_);
    }

    constexpr const T &operator*() const &{
        return value_;
    }

    constexpr T &operator*() &{
        return value_;
    }

    constexpr const T &&operator*() const &&{
        return std::move(value_);
    }

    constexpr T &&operator*() &&{
        return std::move(value_);
    }

    constexpr explicit operator bool() const noexcept {
        return has_value_;
    }

    constexpr bool has_value() const noexcept {
        return has_value_;
    }

    constexpr const T &value() const &{
        if (has_value_) {
            return value_;
        } else {
            throw bad_expected_access(error());
        }
    }

    constexpr T &value() &{
        if (has_value_) {
            return value_;
        } else {
            throw bad_expected_access(error());
        }
    }

    constexpr const T &&value() const &&{
        if (has_value_) {
            return std::move(value_);
        } else {
            throw bad_expected_access(error());
        }
    }

    constexpr T &&value() &&{
        if (has_value_) {
            return std::move(value_);
        } else {
            throw bad_expected_access(error());
        }
    }

    constexpr const E &error() const &{
        return unexpected_.value();
    }

    constexpr E &error() &{
        return unexpected_.value();
    }

    constexpr const E &&error() const &&{
        std::move(unexpected_.value());
    }

    constexpr E &&error() &&{
        std::move(unexpected_.value());
    }

    template<typename U>
    constexpr T value_or(U &&v) const &{
        return has_value_ ? **this : static_cast<T>(std::forward<U>(v));
    }

    template<typename U>
    constexpr T value_or(U &&v) &&{
        return *this ? std::move(**this) : static_cast<T>(std::forward<U>(v));
    }

    template<typename T1, typename E1, typename T2, typename E2>
    friend constexpr bool operator==(const expected<T1, E1> &x, const expected<T2, E2> &y);

    template<typename T1, typename E1, typename T2, typename E2>
    friend constexpr bool operator!=(const expected<T1, E1> &x, const expected<T2, E2> &y);

    template<typename T1, typename E1, typename T2>
    friend constexpr bool operator==(const expected<T1, E1> &x, const T2 &v);

    template<typename T1, typename E1, typename T2>
    friend constexpr bool operator==(const T2 &v, const expected<T1, E1> &x);

    template<typename T1, typename E1, typename T2>
    friend constexpr bool operator!=(const expected<T1, E1> &x, const T2 &v);

    template<typename T1, typename E1, typename T2>
    friend constexpr bool operator!=(const T2 &v, const expected<T1, E1> &x);

    template<typename T1, typename E1, typename E2>
    friend constexpr bool operator==(const expected<T1, E1> &x, const unexpected<E2> &e);

    template<typename T1, typename E1, typename E2>
    friend constexpr bool operator==(const unexpected<E2> &e, const expected<T1, E1> &x);

    template<typename T1, typename E1, typename E2>
    friend constexpr bool operator!=(const expected<T1, E1> &x, const unexpected<E2> &e);

    template<typename T1, typename E1, typename E2>
    friend constexpr bool operator!=(const unexpected<E2> &e, const expected<T1, E1> &x);

    template<typename T1, typename E1>
    friend void swap(expected<T1, E1> &x, expected<T1, E1> &y) noexcept requires (
            std::is_move_constructible_v<T1> &&
            std::is_swappable_v<T1> &&
            std::is_move_constructible_v<E1> &&
            std::is_swappable_v<E1>
    );

private:
    bool has_value_;
    union {
        value_type value_;
        unexpected_type unexpected_;
    };
};

template<typename E>
class expected<void, E> {
public:
    using value_type = void;
    using error_type = E;
    using unexpected_type = unexpected<E>;

    template<typename U>
    using rebind = expected<U, error_type>;

    constexpr expected() : has_value_(true) {};

    constexpr expected(const expected &rhs) requires std::is_copy_constructible_v<E> {
        if (!(has_value_ = rhs.has_value_)) {
            unexpected_ = unexpected(rhs.error());
        }
    }

    constexpr expected(expected &&rhs) noexcept(std::is_nothrow_move_constructible_v<E>) requires (
            std::is_move_constructible_v<void> &&
            std::is_move_constructible_v<E>
    ) {
        if (!(has_value_ = rhs.has_value_)) {
            unexpected_ = unexpected(std::move(rhs.error()));
        }
    }

    template<typename U, typename G>
    explicit(!std::is_convertible_v<const G &, E>) constexpr expected(const expected<U, G> &rhs) requires (
            std::is_void_v<U> || (
                    std::is_constructible_v<void, const U &> &&
                    !std::is_constructible_v<void, expected<U, G> &> &&
                    !std::is_constructible_v<void, expected<U, G> &&> &&
                    !std::is_constructible_v<void, const expected<U, G> &> &&
                    !std::is_constructible_v<void, const expected<U, G> &&> &&
                    !std::is_convertible_v<expected<U, G> &, void> &&
                    !std::is_convertible_v<expected<U, G> &&, void> &&
                    !std::is_convertible_v<const expected<U, G> &, void> &&
                    !std::is_convertible_v<const expected<U, G> &&, void> &&
                    std::is_constructible_v<E, const G &> &&
                    !std::is_constructible_v<unexpected<E>, expected<U, G> &> &&
                    !std::is_constructible_v<unexpected<E>, expected<U, G> &&> &&
                    !std::is_constructible_v<unexpected<E>, const expected<U, G> &> &&
                    !std::is_constructible_v<unexpected<E>, const expected<U, G> &&> &&
                    !std::is_convertible_v<expected<U, G> &, unexpected<E>> &&
                    !std::is_convertible_v<expected<U, G> &&, unexpected<E>> &&
                    !std::is_convertible_v<const expected<U, G> &, unexpected<E>> &&
                    !std::is_convertible_v<const expected<U, G> &&, unexpected<E>>
            )
    ) {
        if (!(has_value_ = rhs.has_value_)) {
            unexpected_ = unexpected(rhs.error());
        }
    }

    template<typename U, typename G>
    explicit(!std::is_convertible_v<G &&, E>)
    constexpr expected(const expected<U, G> &&rhs) requires (
            std::is_void_v<U> || (
                    std::is_constructible_v<void, U &&> &&
                    !std::is_constructible_v<void, expected<U, G> &> &&
                    !std::is_constructible_v<void, expected<U, G> &&> &&
                    !std::is_constructible_v<void, const expected<U, G> &> &&
                    !std::is_constructible_v<void, const expected<U, G> &&> &&
                    !std::is_convertible_v<expected<U, G> &, void> &&
                    !std::is_convertible_v<expected<U, G> &&, void> &&
                    !std::is_convertible_v<const expected<U, G> &, void> &&
                    !std::is_convertible_v<const expected<U, G> &&, void> &&
                    std::is_constructible_v<E, G &&> &&
                    !std::is_constructible_v<unexpected<E>, expected<U, G> &> &&
                    !std::is_constructible_v<unexpected<E>, expected<U, G> &&> &&
                    !std::is_constructible_v<unexpected<E>, const expected<U, G> &> &&
                    !std::is_constructible_v<unexpected<E>, const expected<U, G> &&> &&
                    !std::is_convertible_v<expected<U, G> &, unexpected<E>> &&
                    !std::is_convertible_v<expected<U, G> &&, unexpected<E>> &&
                    !std::is_convertible_v<const expected<U, G> &, unexpected<E>> &&
                    !std::is_convertible_v<const expected<U, G> &&, unexpected<E>>
            )
    ) {
        if (!(has_value_ = rhs.has_value_)) {
            unexpected_ = unexpected(std::move(rhs.error()));
        }
    }

    template<typename G = E>
    explicit(!std::is_convertible_v<const G &, E>)
    constexpr expected(const unexpected<G> &e) requires std::is_constructible_v<E, const G &> {
        unexpected_ = e;
        has_value_ = false;
    }

    template<typename G = E>
    explicit(!std::is_convertible_v<G &&, E>) constexpr expected(unexpected<G> &&e)
    noexcept(std::is_nothrow_constructible_v<E, G &&>) requires std::is_constructible_v<E, G &&> :
            has_value_(false), unexpected_(std::move(e)) {}

    template<typename ...Args>
    constexpr explicit expected(std::in_place_t, Args &&...) requires (sizeof...(Args) == 0) : has_value_(true) {}

    template<typename ...Args>
    constexpr explicit expected(unexpect_t, Args &&...args) requires std::is_constructible_v<E, Args...> :
            has_value_(false), unexpected_(std::forward<Args>(args)...) {}

    template<typename U, typename ...Args>
    constexpr explicit expected(unexpect_t, std::initializer_list<U> list,
                                Args &&...args) requires std::is_constructible_v<E, std::initializer_list<U> &, Args...>
            : has_value_(false), unexpected_(list, std::forward<Args>(args)...) {}

    ~expected() {
        if (!std::is_trivially_destructible_v<E> && !has_value_) {
            unexpected_.~unexpected();
        }
    }

    expected &operator=(const expected &rhs) requires std::is_copy_assignable_v<E> && std::is_copy_constructible_v<E> {
        if (rhs && !has_value_) {
            unexpected_.~unexpected();
        } else if (!rhs) {
            unexpected_ = unexpected(rhs.error());
        }
        has_value_ = rhs.has_value_;
        return *this;
    }

    expected &operator=(expected &&rhs) requires std::is_nothrow_move_constructible_v<E> &&
                                                 std::is_nothrow_move_assignable_v<E> {
        if (rhs && has_value_) {
            unexpected_.~unexpected<E>();
        } else {
            unexpected_ = unexpected(std::move(rhs).error());
        }
        has_value_ = rhs.has_value_;
        return *this;
    }

    template<typename G = E>
    expected<void, E> &operator=(const unexpected<G> &e) requires std::is_nothrow_copy_constructible_v<E> &&
                                                                  std::is_copy_assignable_v<E> {
        unexpected_ = unexpected(e.error());
        has_value_ = false;
    }

    template<typename G = E>
    expected &operator=(unexpected<G> &&e) requires std::is_nothrow_move_constructible_v<E> &&
                                                    std::is_move_assignable_v<E> {
        unexpected_ = unexpected(std::move(e.error()));
        has_value_ = false;
        return *this;
    }

    void emplace() {
        if (!has_value_) {
            unexpected_.~unexpected();
            has_value_ = true;
        }
    }

    void swap(expected<void, E> &rhs) requires std::is_swappable_v<std::remove_reference<E>> {
        if (rhs) {
            if (!*this) {
                rhs.swap(*this);
            }
        } else {
            if (*this) {
                unexpected_ = unexpected(std::move(rhs));
                has_value_ = false;
                rhs.unexpected_.~unexpected<E>();
                rhs.has_value_ = true;
            } else {
                std::swap(unexpected_, rhs.unexpected_);
            }
        }
    }

    constexpr explicit operator bool() noexcept {
        return has_value_;
    }

    constexpr bool has_value() const noexcept {
        return has_value_;
    }

    constexpr void value() const {
        if (!*this) throw bad_expected_access(error());
    }

    constexpr const E &error() const &{
        return unexpected_.value();
    }

    constexpr E &error() &{
        return unexpected_.value();
    }

    constexpr const E &&error() const &&{
        return std::move(unexpected_.value());
    }

    constexpr E &&error() &&{
        return std::move(unexpected_.value());
    }

    template<typename T1, typename E1, typename T2, typename E2>
    friend constexpr bool operator==(const expected<T1, E1> &x, const expected<T2, E2> &y);

    template<typename T1, typename E1, typename T2, typename E2>
    friend constexpr bool operator!=(const expected<T1, E1> &x, const expected<T2, E2> &y);

    template<typename T1, typename E1, typename T2>
    friend constexpr bool operator==(const expected<T1, E1> &x, const T2 &v);

    template<typename T1, typename E1, typename T2>
    friend constexpr bool operator==(const T2 &v, const expected<T1, E1> &x);

    template<typename T1, typename E1, typename T2>
    friend constexpr bool operator!=(const expected<T1, E1> &x, const T2 &v);

    template<typename T1, typename E1, typename T2>
    friend constexpr bool operator!=(const T2 &v, const expected<T1, E1> &x);

    template<typename T1, typename E1, typename E2>
    friend constexpr bool operator==(const expected<T1, E1> &x, const unexpected<E2> &e);

    template<typename T1, typename E1, typename E2>
    friend constexpr bool operator==(const unexpected<E2> &e, const expected<T1, E1> &x);

    template<typename T1, typename E1, typename E2>
    friend constexpr bool operator!=(const expected<T1, E1> &x, const unexpected<E2> &e);

    template<typename T1, typename E1, typename E2>
    friend constexpr bool operator!=(const unexpected<E2> &e, const expected<T1, E1> &x);

    template<typename T1, typename E1>
    friend void swap(expected<T1, E1> &x, expected<T1, E1> &y) noexcept requires (
            std::is_move_constructible_v<T1> &&
            std::is_swappable_v<T1> &&
            std::is_move_constructible_v<E1> &&
            std::is_swappable_v<E1>
    );

private:
    bool has_value_;
    union {
        unexpected<E> unexpected_;
    };
};

template<typename T1, typename E1, typename T2, typename E2>
constexpr bool operator==(const expected<T1, E1> &x, const expected<T2, E2> &y) {
    return x.has_value_ == y.has_value_ && (!x.has_value_ ? x.error() == y.error() : *x == *y);
}

template<typename T1, typename E1, typename T2, typename E2>
constexpr bool operator!=(const expected<T1, E1> &x, const expected<T2, E2> &y) {
    return x.has_value_ != y.has_value_ || (!x.has_value_ ? x.error() != y.error() : *x != *y);
}

template<typename T1, typename E1, typename T2>
constexpr bool operator==(const expected<T1, E1> &x, const T2 &v) {
    return x && *x == v;
}

template<typename T1, typename E1, typename T2>
constexpr bool operator==(const T2 &v, const expected<T1, E1> &x) {
    return x && *x == v;
}

template<typename T1, typename E1, typename T2>
constexpr bool operator!=(const expected<T1, E1> &x, const T2 &v) {
    return x && *x != v;
}

template<typename T1, typename E1, typename T2>
constexpr bool operator!=(const T2 &v, const expected<T1, E1> &x) {
    return x && *x != v;
}

template<typename T1, typename E1, typename E2>
constexpr bool operator==(const expected<T1, E1> &x, const unexpected<E2> &e) {
    return !x && x.error() == e;
}

template<typename T1, typename E1, typename E2>
constexpr bool operator==(const unexpected<E2> &e, const expected<T1, E1> &x) {
    return !x && x.error() == e;
}

template<typename T1, typename E1, typename E2>
constexpr bool operator!=(const expected<T1, E1> &x, const unexpected<E2> &e) {
    return !x && x.error() != e;
}

template<typename T1, typename E1, typename E2>
constexpr bool operator!=(const unexpected<E2> &e, const expected<T1, E1> &x) {
    return !x && x.error() != e;
}

template<typename T1, typename E1>
void swap(expected<T1, E1> &x, expected<T1, E1> &y) noexcept {
    x.swap(y);
}

template<typename E>
class unexpected {
public:
    using error_type = E;

    constexpr unexpected(const unexpected &) = default;

    constexpr unexpected(unexpected &&) = default;

    template<typename Err = E>
    constexpr explicit unexpected(Err &&e) requires (
            std::is_convertible_v<E, Err> &&
            !std::is_same_v<std::remove_cvref_t<Err>, std::in_place_t> &&
            !std::is_same_v<std::remove_cvref_t<Err>, unexpected>
    ) : value_(std::forward<Err>(e)) {}

    template<typename... Args>
    constexpr explicit unexpected(std::in_place_t, Args &&...args) requires(std::is_constructible_v<E, Args...>)
            : value_(std::forward<Args>(args)...) {}

    template<typename U, typename... Args>
    constexpr explicit unexpected(std::in_place_t, std::initializer_list<U> list, Args &&...args) requires(
            std::is_constructible_v<E, std::initializer_list<U> &, Args...>
    ) : value_(list, std::forward<Args>(args)...) {}

    template<typename Err>
    constexpr explicit(!std::is_convertible_v<const Err &, E>)
    unexpected(const unexpected<Err> &e) requires (
            std::is_constructible_v<E, const Err &> &&
            !std::is_constructible_v<E, unexpected<Err> &> &&
            !std::is_constructible_v<E, unexpected<Err>> &&
            !std::is_constructible_v<E, const unexpected<Err> &> &&
            !std::is_constructible_v<E, const unexpected<Err>> &&
            !std::is_convertible_v<unexpected<Err> &, E> &&
            !std::is_convertible_v<unexpected<Err>, E> &&
            !std::is_convertible_v<const unexpected<Err> &, E> &&
            !std::is_convertible_v<const unexpected<Err>, E>
    ) : value_(e.value_) {}

    template<typename Err>
    constexpr explicit(!std::is_convertible_v<Err, E>) unexpected(unexpected<Err> &&e) requires (
            std::is_constructible_v<E, Err> &&
            !std::is_constructible_v<E, unexpected<Err> &> &&
            !std::is_constructible_v<E, unexpected<Err>> &&
            !std::is_constructible_v<E, const unexpected<Err> &> &&
            !std::is_constructible_v<E, const unexpected<Err>> &&
            !std::is_convertible_v<unexpected<Err> &, E> &&
            !std::is_convertible_v<unexpected<Err>, E> &&
            !std::is_convertible_v<const unexpected<Err> &, E> &&
            !std::is_convertible_v<const unexpected<Err>, E>
    ) : value_(std::move(e.value_)) {}

    constexpr unexpected &operator=(const unexpected &) = default;

    constexpr unexpected &operator=(unexpected &&) = default;

    template<typename Err = E>
    constexpr unexpected &operator=(const unexpected<Err> &e) requires(std::is_assignable_v<E, const Err &>) {
        value_ = e.value_;
        return *this;
    }

    template<typename Err = E>
    constexpr unexpected &operator=(unexpected<Err> &&e) requires(std::is_assignable_v<E, Err>) {
        value_ = std::move(e.value_);
        return *this;
    }

    constexpr const E &value() const &{
        return value_;
    }

    constexpr E &value() &{
        return value_;
    }

    constexpr const E &&value() const &&{
        return std::move(value_);
    }

    constexpr E &&value() &&{
        return std::move(value_);
    }

    void swap(unexpected &other) noexcept(std::is_nothrow_swappable_v<E>) requires std::is_swappable_v<E> {
        std::swap(value_, other.value_);
    }

    template<typename E1, typename E2>
    friend constexpr bool operator==(const unexpected<E1> &x, const unexpected<E2> &y);

    template<typename E1, typename E2>
    friend constexpr bool operator!=(const unexpected<E1> &x, const unexpected<E2> &y);

    template<typename E1>
    friend void swap(unexpected<E1> &x, unexpected<E1> &y) noexcept(noexcept(x.swap(y)));

private:
    E value_;
};

template<typename E1, typename E2>
constexpr bool operator==(const unexpected<E1> &x, const unexpected<E2> &y) {
    return x.value_ == y.value_;
}

template<typename E1, typename E2>
constexpr bool operator!=(const unexpected<E1> &x, const unexpected<E2> &y) {
    return x.value_ != y.value_;
}

template<typename E1>
void swap(unexpected<E1> &x, unexpected<E1> &y) noexcept(noexcept(x.swap(y))) {
    x.swap(y);
}

template<>
class bad_expected_access<void> : public std::exception {
public:
    explicit bad_expected_access() {}
};

template<typename E>
class bad_expected_access : public bad_expected_access<void> {
public:
    explicit bad_expected_access(E e) : value_(e) {}

    virtual const char *what() const noexcept override {
        return "bad_expected_access";
    }

    E &error() &{
        return value_;
    }

    const E &error() const &{
        return value_;
    }

    E &&error() &&{
        return std::move(value_);
    }

    const E &&error() const &&{
        return std::move(value_);
    }

private:
    E value_;
};

#endif //EXPECTED_EXPECTED_HPP
