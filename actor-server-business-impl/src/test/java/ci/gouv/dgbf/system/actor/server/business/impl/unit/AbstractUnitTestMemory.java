package ci.gouv.dgbf.system.actor.server.business.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.persistence.query.EntityFinder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public abstract class AbstractUnitTestMemory extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	public static void assertScopeFunction(String identifier,String expectedName) {
		ScopeFunction scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, identifier);
		assertThat(scopeFunction).as(String.format("Scope function %s exists",identifier)).isNotNull();
		assertThat(scopeFunction.getCode()).isEqualTo(identifier);
		assertThat(scopeFunction.getName()).isEqualTo(expectedName);
	}
}