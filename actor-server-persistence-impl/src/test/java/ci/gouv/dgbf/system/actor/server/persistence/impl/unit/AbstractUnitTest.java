package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryManager;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public abstract class AbstractUnitTest extends org.cyk.utility.test.persistence.server.AbstractUnitTest {

	@Override
	protected void callInitialize() {
		ci.gouv.dgbf.system.actor.server.persistence.impl.ApplicationScopeLifeCycleListener.initialize();
	}
	
	@Override
	protected void __listenAfter__() {
		super.__listenAfter__();
		QueryHelper.clear();
		QueryManager.getInstance().clear();
	}
	
	protected static void assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith(String string,String expectedCode) {
		ScopeFunction scopeFunction = ScopeFunctionQuerier.getInstance().readMaxCodeWhereCodeStartsWith(string);
		assertThat(scopeFunction).as("No scope function starting with "+string+" not found").isNotNull();
		assertThat(scopeFunction.getCode()).as(scopeFunction.getCode()+" is not equal to "+expectedCode).isEqualTo(expectedCode);
	}
	
	protected static void assert_scopeFunctionQuerier_readMaxOrderNumberByFunctionCode(String functionCode,Integer expectedOrderNumber) {
		Integer orderNumber = ScopeFunctionQuerier.getInstance().readMaxOrderNumberByFunctionCode(functionCode);
		assertThat(orderNumber).as("Max order number of "+functionCode).isEqualTo(expectedOrderNumber);
	}
}