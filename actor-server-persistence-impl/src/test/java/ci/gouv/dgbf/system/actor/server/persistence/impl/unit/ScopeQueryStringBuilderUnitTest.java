package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeQueryStringBuilder;

public class ScopeQueryStringBuilderUnitTest extends org.cyk.utility.test.AbstractUnitTest {

	@Test
	public void visible(){		
		assertThat(ScopeQueryStringBuilder.Predicate.visible()).isEqualTo("(v.visible IS NULL OR v.visible = true)");
	}
	
	@Test
	public void visibleBy(){
		assertThat(ScopeQueryStringBuilder.Predicate.visibleBy(null)).isEqualTo("v.scope = t AND (v.visible IS NULL OR v.visible = true)");
		assertThat(ScopeQueryStringBuilder.Predicate.visibleBy(":actorCode")).isEqualTo("v.actor.code = :actorCode AND v.scope = t AND (v.visible IS NULL OR v.visible = true)");
	}
	
	@Test
	public void selfVisible() {
		assertThat(ScopeQueryStringBuilder.Predicate.selfVisible(null)).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(ScopeQueryStringBuilder.Predicate.selfVisible()).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.actor.code = :actorCode AND v.scope = t AND (v.visible IS NULL OR v.visible = true))");		
	}
	
	@Test
	public void childVisible() {
		assertThat(ScopeQueryStringBuilder.Predicate.childVisible(AdministrativeUnit.class,AdministrativeUnit.FIELD_SECTION,null))
		.isEqualTo("EXISTS(SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeChild ON actorScope.scope = scopeChild "
				+ "JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = scopeChild WHERE administrativeUnit.section = t)");
		
		assertThat(ScopeQueryStringBuilder.Predicate.childVisible(AdministrativeUnit.class,AdministrativeUnit.FIELD_SECTION))
		.isEqualTo("EXISTS(SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeChild ON actorScope.scope = scopeChild "
				+ "JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = scopeChild WHERE administrativeUnit.section = t AND actorScope.actor.code = :actorCode)");
	}
	
	@Test
	public void parentVisible() {
		assertThat(ScopeQueryStringBuilder.Predicate.parentVisible("Section","AdministrativeUnit",null))
		.isEqualTo("EXISTS(SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeParent ON actorScope.scope = scopeParent JOIN Section section ON section = "
				+ "scopeParent AND EXISTS(SELECT administrativeUnit FROM AdministrativeUnit administrativeUnit WHERE administrativeUnit = t AND administrativeUnit.section = "
				+ "section AND NOT (EXISTS(SELECT actorScopeAdministrativeUnit FROM ActorScope actorScopeAdministrativeUnit WHERE actorScopeAdministrativeUnit.scope = t AND "
				+ "actorScopeAdministrativeUnit.visible IS NOT NULL AND actorScopeAdministrativeUnit.visible = false))))");
		
		assertThat(ScopeQueryStringBuilder.Predicate.parentVisible("Section","AdministrativeUnit"))
		.isEqualTo("EXISTS(SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeParent ON actorScope.scope = scopeParent JOIN Section section ON "
				+ "section = scopeParent WHERE actorScope.actor.code = :actorCode AND EXISTS(SELECT administrativeUnit FROM AdministrativeUnit administrativeUnit WHERE "
				+ "administrativeUnit = t AND administrativeUnit.section = section AND NOT (EXISTS(SELECT actorScopeAdministrativeUnit FROM ActorScope "
				+ "actorScopeAdministrativeUnit WHERE actorScopeAdministrativeUnit.scope = t AND actorScopeAdministrativeUnit.actor.code = :actorCode AND "
				+ "actorScopeAdministrativeUnit.visible IS NOT NULL AND actorScopeAdministrativeUnit.visible = false))))");
	}
	
	@Test
	public void sections() {
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleSection(null)).doesNotContain(":actorCode");
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleSection(":actorCode")).contains(":actorCode");
	}
	
	@Test
	public void administrativeUnits() {
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleAdministrativeUnit(null)).doesNotContain(":actorCode");
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleAdministrativeUnit(":actorCode")).contains(":actorCode");
	}
}