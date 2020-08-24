package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.security.keycloak.Role;
import org.cyk.utility.__kernel__.security.keycloak.RoleManager;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.FunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.FunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;

@ApplicationScoped
public class FunctionBusinessImpl extends AbstractBusinessEntityImpl<Function, FunctionPersistence> implements FunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override @Transactional
	public void importFormKeycloak() {
		Collection<Role> roles = RoleManager.getInstance().read();
		if(CollectionHelper.isEmpty(roles))
			return;
		Collection<Function> functions = null;
		for(Role role : roles) {
			Function function = __inject__(FunctionPersistence.class).readByBusinessIdentifier(role.getName());
			if(function != null)
				continue;
			if(functions == null)
				functions = new ArrayList<Function>();
			functions.add(new Function().setCode(role.getName()).setName(role.getName()));			
		}
		if(CollectionHelper.isEmpty(functions))
			return;
		createMany(functions);
	}

	@Override @Transactional
	public void exportToKeycloak() {
		Collection<Function> functions = EntityReader.getInstance().readMany(Function.class,FunctionQuerier.QUERY_IDENTIFIER_READ);
		if(CollectionHelper.isEmpty(functions))
			return;
		RoleManager.getInstance().saveByNames(functions.stream().map(x -> x.getCode()).collect(Collectors.toList()));
	}
}