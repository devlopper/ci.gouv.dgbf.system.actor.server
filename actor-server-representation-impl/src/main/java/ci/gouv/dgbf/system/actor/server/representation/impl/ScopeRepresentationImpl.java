package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ScopeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ScopeRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeDto> implements ScopeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
