package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import java.util.List;

import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.server.persistence.test.arquillian.AbstractPersistenceArquillianIntegrationTestWithDefaultDeployment;
import org.junit.Test;

import ci.gouv.dgbf.system.actor.server.business.api.AssignmentsBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RejectedAccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

public class BusinessIntegrationTestAssignments extends AbstractPersistenceArquillianIntegrationTestWithDefaultDeployment {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenPostConstruct__() {
		org.cyk.utility.__kernel__.klass.PersistableClassesGetter.COLLECTION.set(java.util.List.of(
				ProfileFunction.class,ProfilePrivilege.class
				,PrivilegeType.class
				,ActorProfile.class,ActorScope.class
				,ScopeFunction.class,ScopeTypeFunction.class
				,Function.class,FunctionType.class
				,Actor.class
				,Profile.class,ProfileType.class,ScopeType.class
				,AccountRequest.class
				,Identity.class
				,RejectedAccountRequest.class
				));
		super.__listenPostConstruct__();
	}
	
	@Override
	protected void __listenBefore__() {
		// TODO Auto-generated method stub
		//super.__listenBefore__();
	}
	
	//@Test
	public void assignments_all(){
		__inject__(AssignmentsBusiness.class).deleteAll();				
		try {
			__inject__(AssignmentsBusiness.class).initialize();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		Assignments model = new Assignments();
		Filter filter = new Filter();
		filter.addField(AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE, "0");		
		//__inject__(AssignmentsBusiness.class).applyModel(model, filter, List.of(Assignments.FIELD_CREDIT_MANAGER_HOLDER));
	}
	
	@Test
	public void assignments_deriveAllValues(){
		try {
			__inject__(AssignmentsBusiness.class).deriveAllValues(null);
		} catch (Exception exception) {
			exception.printStackTrace();
		}
	}
	
	//@Test
	public void assignments_initialize(){
		try {
			__inject__(AssignmentsBusiness.class).initialize();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	//@Test
	public void assignments_applyModel(){
		__inject__(AssignmentsBusiness.class).initialize();
		Assignments model = new Assignments();
		Filter filter = new Filter();
		filter.addField(AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE, "0");
		__inject__(AssignmentsBusiness.class).applyModel(model, filter, List.of(Assignments.FIELD_CREDIT_MANAGER_HOLDER));
	}
}