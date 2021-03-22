package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;

import javax.inject.Inject;

import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityFinder;
import org.jboss.arquillian.junit.InSequence;
import org.junit.Test;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class ScopeFunctionBusinessIT extends AbstractBusinessIT {

	@Inject private ScopeFunctionBusiness scopeFunctionBusiness;
	
    @Test @InSequence(1)
    public void createHolder_assistantShouldBeCreated() {
    	ScopeFunction scopeFunction = new ScopeFunction();
    	scopeFunction.setScopeFromIdentifier("13010222");
    	scopeFunction.setFunctionFromIdentifier("GC");
    	Long count = EntityCounter.getInstance().count(ScopeFunction.class);
		
    	scopeFunctionBusiness.create(scopeFunction);		
		
		assertThat(EntityCounter.getInstance().count(ScopeFunction.class)).isEqualTo(count+2);
    	assertThat(scopeFunction.getCode()).isEqualTo("G100000");
    	assertThat(scopeFunction.getName()).isEqualTo("Gestionnaire de credits DTI");
    	scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, "A1000000");
    	assertThat(scopeFunction.getCode()).isEqualTo("A1000000");
    	assertThat(scopeFunction.getName()).isEqualTo("Assistant gestionnaire de credits DTI");
    	assertThat(scopeFunction.getParentIdentifier()).as("G100000 is parent of A1000000").isEqualTo("G100000");    	
    	assertThat(ScopeFunctionQuerier.getInstance().countByParentsIdentifiers(List.of("G100000"))).as("children of G100000").isEqualTo(1l);
    }
    
    @Test @InSequence(2)
    public void createAssistantOfHolder() {
    	ScopeFunction holder = EntityFinder.getInstance().find(ScopeFunction.class, "G100000");
    	assertThat(holder).isNotNull();
    	ScopeFunction assistant = new ScopeFunction();
    	assistant.setScope(holder.getScope());
    	assistant.setFunctionFromIdentifier("AGC");
    	assistant.setParentIdentifier(holder.getIdentifier());
    	Long count = EntityCounter.getInstance().count(ScopeFunction.class);		
    	
    	scopeFunctionBusiness.create(assistant);
			
		assertThat(EntityCounter.getInstance().count(ScopeFunction.class)).isEqualTo(count+1);
    	assertThat(assistant.getCode()).isEqualTo("A1000001");
    	assertThat(assistant.getName()).isEqualTo("Assistant gestionnaire de credits DTI");
    	
    	assistant = new ScopeFunction();
    	assistant.setScope(holder.getScope());
    	assistant.setFunctionFromIdentifier("AGC");
    	assistant.setParentIdentifier(holder.getIdentifier());
    	count = EntityCounter.getInstance().count(ScopeFunction.class);		
    	
    	scopeFunctionBusiness.create(assistant);
			
		assertThat(EntityCounter.getInstance().count(ScopeFunction.class)).isEqualTo(count+1);
    	assertThat(assistant.getCode()).isEqualTo("A1000002");
    	assertThat(assistant.getName()).isEqualTo("Assistant gestionnaire de credits DTI");
    }
    
    @Test @InSequence(3)
    public void createAssistantOfHolderFromParentIdentifierOnly() {
    	ScopeFunction assistant = new ScopeFunction();
    	assistant.setParentIdentifier("G100000");
    	Long count = EntityCounter.getInstance().count(ScopeFunction.class);		
    	
    	try {
			scopeFunctionBusiness.create(assistant);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
			
		assertThat(EntityCounter.getInstance().count(ScopeFunction.class)).isEqualTo(count+1);
    	assertThat(assistant.getCode()).isEqualTo("A1000003");
    	assertThat(assistant.getName()).isEqualTo("Assistant gestionnaire de credits DTI");
    	
    	assistant = new ScopeFunction();
    	assistant.setParentIdentifier("G100000");
    	count = EntityCounter.getInstance().count(ScopeFunction.class);		
    	
    	scopeFunctionBusiness.create(assistant);
			
		assertThat(EntityCounter.getInstance().count(ScopeFunction.class)).isEqualTo(count+1);
    	assertThat(assistant.getCode()).isEqualTo("A1000004");
    	assertThat(assistant.getName()).isEqualTo("Assistant gestionnaire de credits DTI");
    }
}