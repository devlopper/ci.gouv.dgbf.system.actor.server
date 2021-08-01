package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.VisibilityQueryStringBuilder;

public class VisibilityQueryStringBuilderUnitTest extends org.cyk.utility.test.AbstractUnitTest {

	@Test
	public void join(){		
		assertThat(VisibilityQueryStringBuilder.Predicate.join(Actor.class, null, "v",null)).isEqualTo("v.actor = t");
		assertThat(VisibilityQueryStringBuilder.Predicate.join(Actor.class, false, "v",null)).isEqualTo("v.actor = t");
		assertThat(VisibilityQueryStringBuilder.Predicate.join(Actor.class, true, "v",null)).isEqualTo("v.actor = t AND v.scope.identifier = :scopeIdentifier");		
		assertThat(VisibilityQueryStringBuilder.Predicate.join(Scope.class, null, "v",null)).isEqualTo("v.scope = t");
		assertThat(VisibilityQueryStringBuilder.Predicate.join(Scope.class, false, "v",null)).isEqualTo("v.scope = t");
		assertThat(VisibilityQueryStringBuilder.Predicate.join(Scope.class, true, "v",null)).isEqualTo("v.scope = t AND v.actor.code = :actorCode");
	}
	
	@Test
	public void visible(){		
		assertThat(VisibilityQueryStringBuilder.Predicate.visible()).isEqualTo("(v.visible IS NULL OR v.visible = true)");
		assertThat(VisibilityQueryStringBuilder.Predicate.visible("v",true)).isEqualTo("v.visible IS NOT NULL AND v.visible = false");
	}
	
	@Test
	public void actor_parameter(){		
		assertThat(VisibilityQueryStringBuilder.Predicate.actor(Boolean.TRUE,"p",null)).isEqualTo("p.actor.code = :actorCode");
	}
	
	@Test
	public void actor_variable(){		
		assertThat(VisibilityQueryStringBuilder.Predicate.actor(null,"p",null)).isEqualTo(null);
		assertThat(VisibilityQueryStringBuilder.Predicate.actor(null,"p",true)).isEqualTo("p.actor = t");
		assertThat(VisibilityQueryStringBuilder.Predicate.actor(false,"p",null)).isEqualTo(null);
		assertThat(VisibilityQueryStringBuilder.Predicate.actor(false,"p",true)).isEqualTo("p.actor = t");
		assertThat(VisibilityQueryStringBuilder.Predicate.actor(true,"p",null)).isEqualTo("p.actor.code = :actorCode");
		assertThat(VisibilityQueryStringBuilder.Predicate.actor(true,"p",true)).isEqualTo("p.actor.code = :actorCode");
	}
	
	public void scope_parameter(){		
		assertThat(VisibilityQueryStringBuilder.Predicate.scope(Boolean.TRUE,"p",null)).isEqualTo("p.scope.identifier = :scopeIdentifier");
	}
	
	@Test
	public void scope_variable(){		
		assertThat(VisibilityQueryStringBuilder.Predicate.scope(null,"p",null)).isEqualTo(null);
		assertThat(VisibilityQueryStringBuilder.Predicate.scope(null,"p",true)).isEqualTo("p.scope = t");
		assertThat(VisibilityQueryStringBuilder.Predicate.scope(false,"p",null)).isEqualTo(null);
		assertThat(VisibilityQueryStringBuilder.Predicate.scope(false,"p",true)).isEqualTo("p.scope = t");
		assertThat(VisibilityQueryStringBuilder.Predicate.scope(true,"p",null)).isEqualTo("p.scope.identifier = :scopeIdentifier");
		assertThat(VisibilityQueryStringBuilder.Predicate.scope(true,"p",true)).isEqualTo("p.scope.identifier = :scopeIdentifier");
	}
	
	@Test
	public void visibleBy(){
		//assertThat(VisibilityQueryStringBuilder.Predicate.visibleBy(null,null,"scope")).isEqualTo("v.scope = t AND (v.visible IS NULL OR v.visible = true)");
		//assertThat(VisibilityQueryStringBuilder.Predicate.visibleBy(false,null,"scope")).isEqualTo("v.scope = t AND (v.visible IS NULL OR v.visible = true)");
		//assertThat(VisibilityQueryStringBuilder.Predicate.visibleBy(true,true,"scope")).isEqualTo("v.scope = t AND v.actor.code = :actorCode AND (v.visible IS NULL OR v.visible = true)");
		
		assertThat(VisibilityQueryStringBuilder.Predicate.visibleBy(false,null,"actor")).isEqualTo("v.actor = t AND (v.visible IS NULL OR v.visible = true)");
		//assertThat(VisibilityQueryStringBuilder.Predicate.visibleBy(true,true,"scope")).isEqualTo("v.scope = t AND v.actor.code = :actorCode AND (v.visible IS NULL OR v.visible = true)");
	}
	
	@Test
	public void selfVisible() {
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(Actor.class,null)).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.actor = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(Actor.class,false)).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.actor = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(Actor.class,true)).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.actor = t AND v.scope.identifier = :scopeIdentifier AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(Scope.class,null)).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(Scope.class,false)).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(Scope.class,true)).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND v.actor.code = :actorCode AND (v.visible IS NULL OR v.visible = true))");
		
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(null,null,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(null,false,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(null,true,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND v.actor.code = :actorCode AND (v.visible IS NULL OR v.visible = true))");		
		
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(false,null,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(false,false,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(false,true,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND v.actor.code = :actorCode AND (v.visible IS NULL OR v.visible = true))");		
		
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(Boolean.TRUE,null,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(Boolean.TRUE,false,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND (v.visible IS NULL OR v.visible = true))");
		assertThat(VisibilityQueryStringBuilder.Predicate.selfVisible(Boolean.TRUE,true,"scope")).isEqualTo("EXISTS(SELECT v.identifier FROM ActorScope v WHERE v.scope = t AND v.actor.code = :actorCode AND (v.visible IS NULL OR v.visible = true))");		
	}
	
	@Test
	public void childVisible() {
		assertThat(VisibilityQueryStringBuilder.Predicate.childVisible(Scope.class,null,AdministrativeUnit.class,Section.class))
		.isEqualTo("EXISTS(SELECT _as.identifier FROM ActorScope _as JOIN Scope child ON _as.scope = child "
				+ "JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = child WHERE administrativeUnit.section = t)");
		
		assertThat(VisibilityQueryStringBuilder.Predicate.childVisible(Scope.class,Boolean.TRUE,AdministrativeUnit.class,Section.class))
		.isEqualTo("EXISTS(SELECT _as.identifier FROM ActorScope _as JOIN Scope child ON _as.scope = child "
				+ "JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = child WHERE administrativeUnit.section = t AND _as.actor.code = :actorCode)");
		
		assertThat(VisibilityQueryStringBuilder.Predicate.childVisible((Boolean)null,null,AdministrativeUnit.class,AdministrativeUnit.FIELD_SECTION))
		.isEqualTo("EXISTS(SELECT _as.identifier FROM ActorScope _as JOIN Scope child ON _as.scope = child "
				+ "JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = child WHERE administrativeUnit.section = t)");
		
		assertThat(VisibilityQueryStringBuilder.Predicate.childVisible(Boolean.TRUE,Boolean.TRUE,AdministrativeUnit.class,AdministrativeUnit.FIELD_SECTION))
		.isEqualTo("EXISTS(SELECT _as.identifier FROM ActorScope _as JOIN Scope child ON _as.scope = child "
				+ "JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = child WHERE administrativeUnit.section = t AND _as.actor.code = :actorCode)");
	}
	
	@Test
	public void parentVisible() {
		assertThat(VisibilityQueryStringBuilder.Predicate.parentVisible(Scope.class,null,Section.class,AdministrativeUnit.class))
		.isEqualTo("EXISTS(SELECT p.identifier FROM ActorScope p JOIN Scope s ON p.scope = s JOIN Section section ON section = s WHERE EXISTS(SELECT administrativeUnit FROM"
				+ " AdministrativeUnit administrativeUnit WHERE administrativeUnit = t AND administrativeUnit.section = section AND NOT (EXISTS(SELECT pAdministrativeUnit FROM"
				+ " ActorScope pAdministrativeUnit WHERE pAdministrativeUnit.scope = t AND pAdministrativeUnit.visible IS NOT NULL AND pAdministrativeUnit.visible = false))))");
		
		assertThat(VisibilityQueryStringBuilder.Predicate.parentVisible(Scope.class,Boolean.TRUE,Section.class,AdministrativeUnit.class))
		.isEqualTo("EXISTS(SELECT p.identifier FROM ActorScope p JOIN Scope s ON p.scope = s JOIN Section section ON section = s WHERE p.actor.code = :actorCode AND "
				+ "EXISTS(SELECT administrativeUnit FROM AdministrativeUnit administrativeUnit WHERE administrativeUnit = t AND administrativeUnit.section = section AND NOT "
				+ "(EXISTS(SELECT pAdministrativeUnit FROM ActorScope pAdministrativeUnit WHERE pAdministrativeUnit.scope = t AND pAdministrativeUnit.actor.code = :actorCode "
				+ "AND pAdministrativeUnit.visible IS NOT NULL AND pAdministrativeUnit.visible = false))))");
		
		assertThat(VisibilityQueryStringBuilder.Predicate.parentVisible((Boolean)null,null,Section.class,AdministrativeUnit.class))
		.isEqualTo("EXISTS(SELECT p.identifier FROM ActorScope p JOIN Scope s ON p.scope = s JOIN Section section ON section = s WHERE EXISTS(SELECT administrativeUnit FROM"
				+ " AdministrativeUnit administrativeUnit WHERE administrativeUnit = t AND administrativeUnit.section = section AND NOT (EXISTS(SELECT pAdministrativeUnit FROM"
				+ " ActorScope pAdministrativeUnit WHERE pAdministrativeUnit.scope = t AND pAdministrativeUnit.visible IS NOT NULL AND pAdministrativeUnit.visible = false))))");
		
		assertThat(VisibilityQueryStringBuilder.Predicate.parentVisible(true,true,Section.class,AdministrativeUnit.class))
		.isEqualTo("EXISTS(SELECT p.identifier FROM ActorScope p JOIN Scope s ON p.scope = s JOIN Section section ON section = s WHERE p.actor.code = :actorCode AND "
				+ "EXISTS(SELECT administrativeUnit FROM AdministrativeUnit administrativeUnit WHERE administrativeUnit = t AND administrativeUnit.section = section AND NOT "
				+ "(EXISTS(SELECT pAdministrativeUnit FROM ActorScope pAdministrativeUnit WHERE pAdministrativeUnit.scope = t AND pAdministrativeUnit.actor.code = :actorCode "
				+ "AND pAdministrativeUnit.visible IS NOT NULL AND pAdministrativeUnit.visible = false))))");
	}
	
	@Test
	public void scope_section() {
		assertThat(VisibilityQueryStringBuilder.Predicate.scopeVisible(ScopeType.CODE_SECTION,null,null)).doesNotContain(":actorCode");
		assertThat(VisibilityQueryStringBuilder.Predicate.scopeVisible(ScopeType.CODE_SECTION,Boolean.TRUE,null)).contains(":actorCode");
	}
	
	@Test
	public void scope_section_negate() {
		assertThat(VisibilityQueryStringBuilder.Predicate.scopeVisible(ScopeType.CODE_SECTION,null,true)).doesNotContain(":actorCode");
		assertThat(VisibilityQueryStringBuilder.Predicate.scopeVisible(ScopeType.CODE_SECTION,true,true)).contains(":actorCode");
	}
	
	@Test
	public void sections() {
		assertThat(VisibilityQueryStringBuilder.Predicate.hasVisibleSection(null,null)).doesNotContain(":actorCode");
		assertThat(VisibilityQueryStringBuilder.Predicate.hasVisibleSection(true,null)).contains(":actorCode");
	}
	
	@Test
	public void administrativeUnits() {
		assertThat(VisibilityQueryStringBuilder.Predicate.hasVisibleAdministrativeUnit(null,null)).doesNotContain(":actorCode");
		assertThat(VisibilityQueryStringBuilder.Predicate.hasVisibleAdministrativeUnit(true,null)).contains(":actorCode");
	}
}