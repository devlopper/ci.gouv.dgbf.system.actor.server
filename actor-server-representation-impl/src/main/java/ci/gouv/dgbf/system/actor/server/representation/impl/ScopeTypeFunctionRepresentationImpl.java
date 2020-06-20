package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ScopeTypeFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeTypeFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ScopeTypeFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeTypeFunctionDto> implements ScopeTypeFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
