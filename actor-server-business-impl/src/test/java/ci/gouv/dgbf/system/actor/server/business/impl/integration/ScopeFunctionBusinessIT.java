package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;

import javax.inject.Inject;

import org.cyk.utility.business.server.EntityUpdater;
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
    	
		scopeFunctionBusiness.create(assistant);
					
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
    
    @Test @InSequence(4)
    public void createHolder_codeShouldBeSequential() {
    	ScopeFunction scopeFunction = new ScopeFunction();
    	scopeFunction.setScopeFromIdentifier("13010222");
    	scopeFunction.setFunctionFromIdentifier("GC");
    	Long count = EntityCounter.getInstance().count(ScopeFunction.class);
		
    	scopeFunctionBusiness.create(scopeFunction);		
		
		assertThat(EntityCounter.getInstance().count(ScopeFunction.class)).isEqualTo(count+2);
    	assertThat(scopeFunction.getCode()).isEqualTo("G100001");
    	assertThat(scopeFunction.getName()).isEqualTo("Gestionnaire de credits DTI");
    	scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, "A1000010");
    	assertThat(scopeFunction.getCode()).isEqualTo("A1000010");
    	assertThat(scopeFunction.getName()).isEqualTo("Assistant gestionnaire de credits DTI");
    	assertThat(scopeFunction.getParentIdentifier()).as("G100001 is parent of A1000010").isEqualTo("G100001");    	
    	assertThat(ScopeFunctionQuerier.getInstance().countByParentsIdentifiers(List.of("G100001"))).as("children of G100001").isEqualTo(1l);
    	
    	EntityUpdater.getInstance().updateMany(EntityFinder.getInstance().find(ScopeFunction.class, "G100001").setCode("G100002"));
    	
    	scopeFunction = new ScopeFunction();
    	scopeFunction.setCodePrefix("G1");
    	scopeFunction.setScopeFromIdentifier("13010222");
    	scopeFunction.setFunctionFromIdentifier("GC");
    	count = EntityCounter.getInstance().count(ScopeFunction.class);
    	
    	scopeFunctionBusiness.create(scopeFunction);
    	
    	assertThat(EntityCounter.getInstance().count(ScopeFunction.class)).isEqualTo(count+2);
    	assertThat(scopeFunction.getCode()).isEqualTo("G100003");
    	assertThat(scopeFunction.getName()).isEqualTo("Gestionnaire de credits DTI");
    	scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, "A1000030");
    	assertThat(scopeFunction.getCode()).isEqualTo("A1000030");
    	assertThat(scopeFunction.getName()).isEqualTo("Assistant gestionnaire de credits DTI");
    	assertThat(scopeFunction.getParentIdentifier()).as("G100003 is parent of A1000030").isEqualTo("G100003");    	
    	assertThat(ScopeFunctionQuerier.getInstance().countByParentsIdentifiers(List.of("G100003"))).as("children of G100003").isEqualTo(1l);
    }
    
    @Test @InSequence(5)
    public void createHolder_authorizingOfficer() {
    	ScopeFunction scopeFunction = new ScopeFunction().setScopeFromIdentifier("ORD001").setFunctionFromIdentifier("OD").setCodePrefix("O3")
    			.setName("Ordonnateur secondaire budget dabou");
		 	
    	scopeFunctionBusiness.create(scopeFunction);
		
		assertThat(scopeFunction.getCode()).isEqualTo("O300000");
    	assertThat(scopeFunction.getName()).isEqualTo("Ordonnateur secondaire budget dabou");
    	
    	scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, "A2000000");
    	assertThat(scopeFunction.getCode()).isEqualTo("A2000000");
    	assertThat(scopeFunction.getName()).isEqualTo("Assistant Ordonnateur secondaire budget dabou");
    	
    	scopeFunction = new ScopeFunction().setScopeFromIdentifier("ORD001").setFunctionFromIdentifier("OD").setCodePrefix("O3")
    			.setLocalityFromIdentifier("DIMBOKRO");
		 	
    	scopeFunctionBusiness.create(scopeFunction);
		
		assertThat(scopeFunction.getCode()).isEqualTo("O300001");
    	assertThat(scopeFunction.getName()).isEqualTo("Ordonnateur secondaire du Programme Budget a Dimbokro");
    	
    	scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, "A2000010");
    	assertThat(scopeFunction.getCode()).isEqualTo("A2000010");
    	assertThat(scopeFunction.getName()).isEqualTo("Assistant ordonnateur secondaire du Programme Budget a Dimbokro");
    }
}