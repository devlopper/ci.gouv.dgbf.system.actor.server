package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeQueryStringBuilder;

public class ScopeQueryStringBuilderUnitTest extends org.cyk.utility.test.AbstractUnitTest {

	@Test
	public void isVisible(){		
		assertThat(ScopeQueryStringBuilder.Predicate.isVisible()).isEqualTo("(v.visible IS NULL OR v.visible = true)");
	}
	
	@Test
	public void isVisibleByActor(){
		assertThat(ScopeQueryStringBuilder.Predicate.isVisibleByActor(":actorCode")).isEqualTo("v.actor.code = :actorCode AND v.scope = t AND (v.visible IS NULL OR v.visible = true)");
	}
	
	@Test
	public void hasVisibility() {
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibility()).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.actor.code = :actorCode AND v.scope = t AND (v.visible IS NULL OR v.visible = true))");
	}
	
	@Test
	public void hasVisibeChild() {
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleChild(AdministrativeUnit.class,AdministrativeUnit.FIELD_SECTION))
		.isEqualTo("EXISTS(SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeChild ON actorScope.scope = scopeChild "
				+ "JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = scopeChild WHERE administrativeUnit.section = t AND actorScope.actor.code = :actorCode)");
	}
	
	@Test
	public void hasVisibeParent() {
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleParent("Section","AdministrativeUnit"))
		.isEqualTo("EXISTS(SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeParent ON actorScope.scope = scopeParent JOIN Section section ON "
				+ "section = scopeParent WHERE actorScope.actor.code = :actorCode AND EXISTS(SELECT administrativeUnit FROM AdministrativeUnit administrativeUnit WHERE "
				+ "administrativeUnit = t AND administrativeUnit.section = section AND NOT (EXISTS(SELECT actorScopeAdministrativeUnit FROM ActorScope "
				+ "actorScopeAdministrativeUnit WHERE actorScopeAdministrativeUnit.scope = t AND actorScopeAdministrativeUnit.actor.code = :actorCode AND "
				+ "actorScopeAdministrativeUnit.visible IS NOT NULL AND actorScopeAdministrativeUnit.visible = false))))");
	}
	
	//@Test
	public void buildPredicateAdministrativeUnitHasVisibleSection() {
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleParent("Section", "administrativeUnit"))
			.isEqualTo("EXISTS(SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeParent ON actorScope.scope = scopeParent JOIN Section section "
					+ "ON section = scopeParent WHERE actorScope.actor.code = :actorCode AND EXISTS(SELECT administrativeUnit FROM administrativeUnit administrativeUnit "
					+ "WHERE administrativeUnit = scope AND administrativeUnit.section = section AND NOT (EXISTS(SELECT actorScopeadministrativeUnit "
					+ "FROM ActorScope actorScopeadministrativeUnit WHERE actorScopeadministrativeUnit.scope = scope AND actorScopeadministrativeUnit.actor.code = :actorCode "
					+ "AND actorScopeadministrativeUnit.visible IS NOT NULL AND actorScopeadministrativeUnit.visible = false))))");
	}
}