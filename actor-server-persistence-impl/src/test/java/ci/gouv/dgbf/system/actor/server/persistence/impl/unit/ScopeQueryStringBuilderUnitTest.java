package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeQueryStringBuilder;

public class ScopeQueryStringBuilderUnitTest extends org.cyk.utility.test.AbstractUnitTest {

	@Test
	public void visible(){		
		assertThat(ScopeQueryStringBuilder.Predicate.visible()).isEqualTo("(v.visible IS NULL OR v.visible = true)");
	}
	
	@Test
	public void actor_parameter(){		
		assertThat(ScopeQueryStringBuilder.Predicate.actor(Boolean.TRUE,"p",null)).isEqualTo("p.actor.code = :actorCode");
	}
	
	@Test
	public void actor_variable(){		
		assertThat(ScopeQueryStringBuilder.Predicate.actor(null,"p",null)).isEqualTo(null);
		assertThat(ScopeQueryStringBuilder.Predicate.actor(null,"p",true)).isEqualTo("p.actor = t");
		assertThat(ScopeQueryStringBuilder.Predicate.actor(false,"p",null)).isEqualTo(null);
		assertThat(ScopeQueryStringBuilder.Predicate.actor(false,"p",true)).isEqualTo("p.actor = t");
		assertThat(ScopeQueryStringBuilder.Predicate.actor(true,"p",null)).isEqualTo("p.actor.code = :actorCode");
		assertThat(ScopeQueryStringBuilder.Predicate.actor(true,"p",true)).isEqualTo("p.actor.code = :actorCode");
	}
	
	@Test
	public void visibleBy(){
		assertThat(ScopeQueryStringBuilder.Predicate.visibleBy(null,null,"scope")).isEqualTo("v.scope = t AND (v.visible IS NULL OR v.visible = true)");
		assertThat(ScopeQueryStringBuilder.Predicate.visibleBy(false,null,"scope")).isEqualTo("v.scope = t AND (v.visible IS NULL OR v.visible = true)");
		//assertThat(ScopeQueryStringBuilder.Predicate.visibleBy(true,null,"scope")).isEqualTo("v.scope = t AND (v.visible IS NULL OR v.visible = true)");
		
		assertThat(ScopeQueryStringBuilder.Predicate.visibleBy(false,null,"actor")).isEqualTo("v.actor = t AND (v.visible IS NULL OR v.visible = true)");
		assertThat(ScopeQueryStringBuilder.Predicate.visibleBy(true,true,"scope")).isEqualTo("v.scope = t AND v.actor.code = :actorCode AND (v.visible IS NULL OR v.visible = true)");
	}
	
	@Test
	public void selfVisible() {
		assertThat(ScopeQueryStringBuilder.Predicate.selfVisible(null,Boolean.TRUE,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(ScopeQueryStringBuilder.Predicate.selfVisible(true,Boolean.TRUE,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND v.actor.code = :actorCode AND (v.visible IS NULL OR v.visible = true))");		
	}
	
	@Test
	public void childVisible() {
		assertThat(ScopeQueryStringBuilder.Predicate.childVisible(null,null,AdministrativeUnit.class,AdministrativeUnit.FIELD_SECTION))
		.isEqualTo("EXISTS(SELECT _as.identifier FROM ActorScope _as JOIN Scope child ON _as.scope = child "
				+ "JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = child WHERE administrativeUnit.section = t)");
		
		assertThat(ScopeQueryStringBuilder.Predicate.childVisible(Boolean.TRUE,Boolean.TRUE,AdministrativeUnit.class,AdministrativeUnit.FIELD_SECTION))
		.isEqualTo("EXISTS(SELECT _as.identifier FROM ActorScope _as JOIN Scope child ON _as.scope = child "
				+ "JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = child WHERE administrativeUnit.section = t AND _as.actor.code = :actorCode)");
	}
	
	@Test
	public void parentVisible() {
		assertThat(ScopeQueryStringBuilder.Predicate.parentVisible(null,null,Section.class,AdministrativeUnit.class))
		.isEqualTo("EXISTS(SELECT p.identifier FROM ActorScope p JOIN Scope s ON p.scope = s JOIN Section section ON section = s WHERE EXISTS(SELECT administrativeUnit FROM"
				+ " AdministrativeUnit administrativeUnit WHERE administrativeUnit = t AND administrativeUnit.section = section AND NOT (EXISTS(SELECT pAdministrativeUnit FROM"
				+ " ActorScope pAdministrativeUnit WHERE pAdministrativeUnit.scope = t AND pAdministrativeUnit.visible IS NOT NULL AND pAdministrativeUnit.visible = false))))");
		
		assertThat(ScopeQueryStringBuilder.Predicate.parentVisible(true,true,Section.class,AdministrativeUnit.class))
		.isEqualTo("EXISTS(SELECT p.identifier FROM ActorScope p JOIN Scope s ON p.scope = s JOIN Section section ON section = s WHERE p.actor.code = :actorCode AND "
				+ "EXISTS(SELECT administrativeUnit FROM AdministrativeUnit administrativeUnit WHERE administrativeUnit = t AND administrativeUnit.section = section AND NOT "
				+ "(EXISTS(SELECT pAdministrativeUnit FROM ActorScope pAdministrativeUnit WHERE pAdministrativeUnit.scope = t AND pAdministrativeUnit.actor.code = :actorCode "
				+ "AND pAdministrativeUnit.visible IS NOT NULL AND pAdministrativeUnit.visible = false))))");
	}
	
	@Test
	public void scope_section() {
		assertThat(ScopeQueryStringBuilder.Predicate.scopeVisible(ScopeType.CODE_SECTION,null,null,null)).doesNotContain(":actorCode");
		assertThat(ScopeQueryStringBuilder.Predicate.scopeVisible(ScopeType.CODE_SECTION,true,Boolean.TRUE,null)).contains(":actorCode");
	}
	
	@Test
	public void scope_section_negate() {
		assertThat(ScopeQueryStringBuilder.Predicate.scopeVisible(ScopeType.CODE_SECTION,null,null,true)).doesNotContain(":actorCode");
		assertThat(ScopeQueryStringBuilder.Predicate.scopeVisible(ScopeType.CODE_SECTION,true,true,true)).contains(":actorCode");
	}
	
	@Test
	public void sections() {
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleSection(null,null,null)).doesNotContain(":actorCode");
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleSection(true,true,null)).contains(":actorCode");
	}
	
	@Test
	public void administrativeUnits() {
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleAdministrativeUnit(null,null,null)).doesNotContain(":actorCode");
		assertThat(ScopeQueryStringBuilder.Predicate.hasVisibleAdministrativeUnit(true,true,null)).contains(":actorCode");
	}
}